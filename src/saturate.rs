use super::*;

impl VarFrag {
    fn get_slice<'a>(self, var_chunks: &'a [&'a [u8]]) -> &'a [u8] {
        // assumes in bounds!
        &var_chunks[self.var_idx.0 as usize]
            [self.var_bytes.start as usize..self.var_bytes.end as usize]
    }
}
impl State {
    fn frag_slice<'a, 'b>(&'a self, frag: &'b Frag, var_chunks: &'a [&'a [u8]]) -> &'a [u8] {
        &(match frag.source {
            FragSource::Var(vid) => &var_chunks[vid.0 as usize],
            FragSource::Const => self.const_bytes.as_slice(),
        }[frag.bytes_range.word_range()])
    }
    pub(crate) fn generate_all(&mut self) {
        let mut byte_buf: Vec<u8> = vec![];
        'rules: for state_rule in self.state_rules.iter() {
            let mut var_arenas_combo =
                ChunkCombo::new(state_rule.var_types.iter().map(|tid| self.pos.get(tid).unwrap()));
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
