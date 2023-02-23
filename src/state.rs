use super::*;

impl VarFrag {
    fn get_slice<'a>(self, var_chunks: &'a [&'a [u8]]) -> &'a [u8] {
        // assumes in bounds!
        &var_chunks[self.var_idx.0 as usize]
            [self.var_bytes.start as usize..self.var_bytes.end as usize]
    }
}
impl State {
    pub(crate) fn advance(&mut self) {
        self.prev_prev_pos = self.prev_pos.replace(std::mem::take(&mut self.pos));
    }
    pub(crate) fn pos_and_prev_prev_eq(&self) -> bool {
        if let Some(prev_prev_pos) = self.prev_prev_pos.as_ref() {
            prev_prev_pos == &self.pos
        } else {
            false // NOT interested in trivial case of <2 advance() calls
        }
    }
    fn frag_slice<'a, 'b>(&'a self, frag: &'b Frag, var_chunks: &'a [&'a [u8]]) -> &'a [u8] {
        &(match frag.source {
            FragSource::Var(vid) => &var_chunks[vid.0 as usize],
            FragSource::Const => self.const_bytes.as_slice(),
        }[frag.bytes_range.word_range()])
    }
    pub(crate) fn saturate(&mut self) {
        let mut byte_buf: Vec<u8> = vec![];
        'rules: for state_rule in self.state_rules.iter() {
            let maybe_var_arenas = state_rule.var_types.iter().map(|tid| self.pos.get(tid));
            if maybe_var_arenas.clone().any(|maybe_arena| maybe_arena.is_none()) {
                continue 'rules;
            }
            let mut var_arenas_combo = ChunkCombo::new(maybe_var_arenas.map(Option::unwrap));
            'combo: while let Some(var_chunks) = var_arenas_combo.next() {
                // cmp checks
                for cmp_check in state_rule.frag_cmp_checks.iter() {
                    let slice_a = self.frag_slice(&cmp_check.frag_a, var_chunks);
                    let slice_b = self.frag_slice(&cmp_check.frag_b, var_chunks);
                    let pass = match cmp_check.cmp_kind {
                        CmpKind::Eq => slice_a == slice_b,
                        CmpKind::Neq => slice_a != slice_b,
                        CmpKind::Lt => chunk_cmp(slice_a, slice_b) == Ordering::Less,
                        CmpKind::Leq => chunk_cmp(slice_a, slice_b) != Ordering::Greater,
                    };
                    if !pass {
                        continue 'combo;
                    }
                }
                // lit checks
                for lit_check in state_rule.lit_checks.iter() {
                    byte_buf.clear();

                    for frag in lit_check.frags.iter() {
                        byte_buf.extend(self.frag_slice(frag, var_chunks));
                        if lit_check.pos {
                            // checking known truth of this atom
                            let contained = self
                                .pos
                                .get(&lit_check.tid)
                                .expect("unknown TID")
                                .contains(&byte_buf)
                                .expect("wrong len");
                            if !contained {
                                println!("lol nevermind. literal should be positive, but it isn't");
                                continue 'combo;
                            }
                        } else {
                            // checking known falsity of this atom
                            if let Some(prev_pos) = self.prev_pos.as_ref() {
                                let contained = prev_pos
                                    .get(&lit_check.tid)
                                    .expect("unknown TID")
                                    .contains(&byte_buf)
                                    .expect("wrong len");
                                if contained {
                                    // this particlar atom is NOT known to be false
                                    continue 'combo;
                                }
                            } else {
                                // NO known false atoms
                                continue 'combo;
                            }
                        }
                    }
                    // whee
                }
                // results
                byte_buf.clear();
                for frag in state_rule.result_frags.iter() {
                    byte_buf.extend(self.frag_slice(frag, var_chunks));
                }
                if self
                    .pos
                    .get(&state_rule.result_tid)
                    .expect("unknown TID")
                    .contains(&byte_buf)
                    .expect("wrong len")
                {
                    println!("lol nevermind. already get this result");
                } else {
                    println!("new result!");
                    self.pos.get_mut(&state_rule.result_tid).unwrap().insert(&byte_buf).unwrap();
                    continue 'rules;
                }
            }
        }
    }
}

impl std::fmt::Debug for PrintableStateInterpretation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let t = self
            .0
            .pos
            .iter()
            .flat_map(|(tid, arena)| arena.iter().map(move |chunk| ((tid, chunk), 'T')));
        if let Some(prev_pos) = self.0.prev_pos.as_ref() {
            let u = prev_pos.iter().flat_map(|(tid, arena)| {
                arena
                    .iter()
                    .filter(|chunk| {
                        let b: bool = self
                            .0
                            .pos
                            .get(tid)
                            .map(|pos_arena| pos_arena.contains(chunk).expect("bad len"))
                            .unwrap_or(false);
                        b
                    })
                    .map(move |chunk| ((tid, chunk), '?'))
            });
            // let u = prev_pos.set_minus_iter(&self.0.pos).map(|atom| (atom, '?'));
            f.debug_map().entries(t.chain(u)).finish()
        } else {
            let u = None;
            f.debug_map().entries(t.chain(u)).finish()
        }
    }
}

pub(crate) fn test() {
    let mut state = State {
        state_rules: vec![
            StateRule {
                var_types: vec![TypeId(0), TypeId(1)],
                frag_cmp_checks: vec![
                    FragCmpCheck {
                        frag_a: Frag { bytes_range: 0..1, source: FragSource::Var(VarIdx(0)) },
                        frag_b: Frag { bytes_range: 0..1, source: FragSource::Var(VarIdx(1)) },
                        cmp_kind: CmpKind::Leq,
                    }, // whee
                ],
                lit_checks: vec![],
                result_tid: TypeId(2),
                result_frags: vec![
                    Frag { bytes_range: 0..1, source: FragSource::Var(VarIdx(0)) },
                    Frag { bytes_range: 0..1, source: FragSource::Var(VarIdx(0)) },
                    Frag { bytes_range: 0..1, source: FragSource::Var(VarIdx(1)) },
                    Frag { bytes_range: 0..1, source: FragSource::Var(VarIdx(1)) },
                ],
            }, //whee
        ],
        pos: hashmap! {
            TypeId(0) => ChunkArena::from_slice([[1], [2], [3]].iter()),
            TypeId(1) => ChunkArena::from_slice([[2], [3], [4]].iter()),
            TypeId(2) => ChunkArena::from_slice([[1,1,1,1]].iter()),
        },
        prev_pos: None,
        prev_prev_pos: None,
        const_bytes: vec![],
    };
    let mut iterations = 0;
    loop {
        // println!("ADVANCED {:#?}", &kb);
        state.saturate();
        // println!("SATURATED {:#?}", &kb);
        if iterations % 2 == 0 && state.pos_and_prev_prev_eq() {
            break;
        }
        state.advance();
        iterations += 1;
    }
    println!("iterations {:?}", iterations);
    println!("{:#?}", PrintableStateInterpretation(&state));
    // state.saturate();
    // println!("{:#?}", &state.pos);
    // state.saturate();
    // println!("{:#?}", &state.pos);
}
