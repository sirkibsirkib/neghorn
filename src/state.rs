use super::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct TypeId(u32);

#[derive(Debug)]
struct TypeDef {
    label: &'static str,
    fields: Vec<TypeId>,
}

#[derive(Debug)]
struct TypeInfo {
    type_defs: HashMap<TypeId, TypeDef>,
    type_size: HashMap<TypeId, usize>, // sum of all
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum TidSizeErr {
    RecursiveDepthExceeded,
    DependsOnSizeOfUnknown(TypeId),
}

enum CmpKind {
    Leq,
    Lt,
    Eq,
    Neq,
}
#[derive(Debug, Clone, Copy)]
struct VarIdx(u8);

#[derive(Debug, Clone)]
struct VarFrag {
    var_idx: VarIdx,
    var_bytes: Range<u16>,
}
#[derive(Debug, Clone)]
enum FragSource {
    Const,
    Var(VarIdx),
}
#[derive(Debug, Clone)]
struct Frag {
    bytes_range: Range<u16>,
    source: FragSource,
}
struct FragCmpCheck {
    // assumes ranges are in bounds AND same length
    frag_a: Frag,
    frag_b: Frag,
    cmp_kind: CmpKind,
}
struct ReturnInfo {
    built_range: Range<u16>,
    tid: TypeId,
}
struct LitCheck {
    frags: Vec<Frag>,
    tid: TypeId,
    pos: bool,
}
struct StateRule {
    var_types: Vec<TypeId>, // will consider all quantifications
    frag_cmp_checks: Vec<FragCmpCheck>,
    lit_checks: Vec<LitCheck>,
    result_tid: TypeId,
    result_frags: Vec<Frag>,
}
struct State {
    state_rules: Vec<StateRule>,
    pos: HashMap<TypeId, ChunkArena>,
    prev_pos: Option<HashMap<TypeId, ChunkArena>>,
    prev_prev_pos: Option<HashMap<TypeId, ChunkArena>>,
    const_bytes: Vec<u8>,
    type_info: TypeInfo,
}
struct PrintableStateInterpretation<'a>(&'a State);

/////////////////
impl TypeId {
    const U8: Self = Self(0);
    const U4: Self = Self(1);
    const I4: Self = Self(2);
    const fn const_size(self) -> Option<usize> {
        Some(match self {
            Self::U8 => 1,
            Self::U4 | Self::I4 => 4,
            _ => return None,
        })
    }
}
impl TypeInfo {
    fn new(type_defs: HashMap<TypeId, TypeDef>) -> Result<Self, ()> {
        let mut size_todo: HashSet<TypeId> = type_defs.keys().copied().collect();
        let mut type_size: HashMap<TypeId, usize> = Default::default();

        // invariant: type_size.keys() disjoint with size_todo.
        const MAX_DEPTH: u8 = 100;

        'arb: while let Some(mut tid) = size_todo.iter().copied().next() {
            'tid_compute: for _ in 0..MAX_DEPTH {
                assert!(tid.const_size().is_none());
                let def = type_defs.get(&tid).ok_or(())?;
                let r: Result<usize, TypeId> = def.fields.iter().fold(Ok(0), |acc, field| {
                    Ok(acc?
                        + field
                            .const_size()
                            .or_else(|| type_size.get(field).copied())
                            .ok_or(*field)?)
                });
                match r {
                    Ok(size) => {
                        size_todo.remove(&tid);
                        type_size.insert(tid, size);
                        continue 'arb;
                    }
                    Err(field) => {
                        tid = field;
                        continue 'tid_compute;
                    }
                };
            }
            return Err(());
        }
        Ok(Self { type_defs, type_size })
    }
}

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
    let type_info = TypeInfo::new(hashmap! {
        TypeId(5) => TypeDef { label: "person", fields: vec![TypeId::U8], }
    })
    .expect("weh");
    println!("{:#?}", &type_info);
    return;
    let mut state = State {
        type_info,
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
