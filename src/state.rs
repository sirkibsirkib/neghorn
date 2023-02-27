use super::*;

#[derive(Debug, Copy, Clone)]
enum CmpOp {
    Leq,
    Lt,
    Eq,
    Neq,
}
#[derive(Debug, Clone, Copy)]
struct VarIdx(u8);

#[derive(Debug)]
struct ResInfo {
    tid: TypeId,
    res_src: ResSrc,
    offset: u16,
}
#[derive(Debug)]
enum ResSrc {
    Const,
    Buf,
}
#[derive(Debug)]
struct StateRule {
    var_types: Vec<TypeId>, // will consider all quantifications
    rule_ins: Vec<RuleIns>,
    res_info: ResInfo,
}

#[derive(Copy, Clone, Debug)]
enum Sign {
    Pos,
    Neg,
}

#[derive(Debug, Copy, Clone)]
enum FragSrcKind {
    Const,
    Buf,
    Var { var_id: VarIdx },
}
#[derive(Debug, Copy, Clone)]
struct FragSrc {
    kind: FragSrcKind,
    offset: u16,
}
#[derive(Debug, Copy, Clone)]
enum ExtendSrc {
    Var { var_id: VarIdx },
    Const,
}
#[derive(Debug, Clone)]
enum RuleIns {
    ExtendBuf { src_range: Range<u16>, extend_src: ExtendSrc },
    Truncate { new_len: u16 },
    CheckCmp { frag_srcs: [FragSrc; 2], len: u16, op: CmpOp },
    LitCheck { sign: Sign, frag_src: FragSrc, tid: TypeId },
}
#[derive(Debug)]
struct StateR {
    state_rules: Vec<StateRule>,
    const_bytes: Vec<u8>,
    type_map: TypeMap,
}
#[derive(Debug)]
struct State {
    state_r: StateR,
    pos: HashMap<TypeId, ChunkArena>,
    prev_pos: Option<HashMap<TypeId, ChunkArena>>,
    prev_prev_pos: Option<HashMap<TypeId, ChunkArena>>,
}
struct PrintableStateInterpretation<'a>(&'a State);
struct PrintableAtom<'a> {
    state: &'a State,
    tid: TypeId,
    chunk: &'a [u8],
}

/////////////////

impl FragSrc {
    fn slice<'a>(
        self,
        const_bytes: &'a Vec<u8>,
        var_chunks: &'a [&'a [u8]],
        byte_buf: &'a Vec<u8>,
        len: usize,
    ) -> &'a [u8] {
        let FragSrc { offset, kind } = self;
        let src_slice = match kind {
            FragSrcKind::Const => const_bytes.as_slice(),
            FragSrcKind::Buf => byte_buf.as_slice(),
            FragSrcKind::Var { var_id } => &var_chunks[var_id.0 as usize],
        };
        &src_slice[offset as usize..(offset as usize + len)]
    }
}
impl StateR {
    fn execute_ins_prepare_buf(
        &self,
        pos: &HashMap<TypeId, ChunkArena>,
        prev_pos: &Option<HashMap<TypeId, ChunkArena>>,
        rule_ins: &[RuleIns],
        var_chunks: &[&[u8]],
        byte_buf: &mut Vec<u8>,
    ) -> bool {
        for instruction in rule_ins.iter().cloned() {
            match instruction {
                RuleIns::Truncate { new_len } => byte_buf.truncate(new_len as usize),
                RuleIns::ExtendBuf { extend_src, src_range } => {
                    let src_slice = match extend_src {
                        ExtendSrc::Const => self.const_bytes.as_slice(),
                        ExtendSrc::Var { var_id } => &var_chunks[var_id.0 as usize],
                    };
                    byte_buf.extend(&src_slice[src_range.clone().word_range()])
                }
                RuleIns::CheckCmp { frag_srcs: [a, b], len, op } => {
                    if !op.cmp_check(
                        a.slice(&self.const_bytes, var_chunks, byte_buf, len as usize),
                        b.slice(&self.const_bytes, var_chunks, byte_buf, len as usize),
                    ) {
                        return false;
                    }
                }
                RuleIns::LitCheck { sign, frag_src: FragSrc { offset, kind }, tid } => {
                    let src_slice = match kind {
                        FragSrcKind::Const => self.const_bytes.as_slice(),
                        FragSrcKind::Buf => byte_buf.as_slice(),
                        FragSrcKind::Var { var_id } => &var_chunks[var_id.0 as usize],
                    };
                    let len = self.type_map.lookup_type_size(tid).expect("IDK this type");
                    let slice = &src_slice[offset as usize..(offset as usize + len)];
                    if !self.known(pos, prev_pos, sign, tid, slice).expect("wrong size eh") {
                        return false;
                    }
                }
            }
        }
        true
    }
    fn known(
        &self,
        pos: &HashMap<TypeId, ChunkArena>,
        prev_pos: &Option<HashMap<TypeId, ChunkArena>>,
        sign: Sign,
        tid: TypeId,
        chunk: &[u8],
    ) -> Result<bool, WrongSize> {
        let in_arena = |arenas: &HashMap<TypeId, ChunkArena>| match arenas.get(&tid) {
            Some(arena) => arena.contains(chunk),
            None => Ok(false),
        };
        match sign {
            Sign::Pos => {
                // presence of X in existing arena <=> X is KNOWN
                in_arena(pos)
            }
            Sign::Neg => {
                if let Some(arenas) = prev_pos.as_ref() {
                    // presence of X in existing arena <=> X is UNKNOWN
                    in_arena(arenas).map(Not::not)
                } else {
                    // lack of an arena altogether <=> X UNKNOWN for all X
                    Ok(false)
                }
            }
        }
    }
}

impl CmpOp {
    fn cmp_check(self, a: &[u8], b: &[u8]) -> bool {
        match self {
            Self::Eq => a == b,
            Self::Neq => a != b,
            Self::Lt => chunk_cmp(a, b) == Ordering::Less,
            Self::Leq => chunk_cmp(a, b) != Ordering::Greater,
        }
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

    fn add_pos(
        state_r: &StateR,
        pos: &mut HashMap<TypeId, ChunkArena>,
        tid: TypeId,
        chunk: &[u8],
    ) -> Result<(), WrongSize> {
        pos.entry(tid)
            .or_insert_with(|| {
                ChunkArena::new(state_r.type_map.lookup_type_size(tid).expect("idk fam"))
            })
            .insert(chunk)
            .map(drop)
    }

    pub(crate) fn saturate(&mut self) {
        let mut byte_buf: Vec<u8> = vec![];
        let byte_buf = &mut byte_buf;
        let Self { state_r, pos, prev_pos, .. } = self;
        'all_rules: loop {
            'rules: for state_rule in state_r.state_rules.iter() {
                let maybe_var_arenas = state_rule.var_types.iter().map(|tid| pos.get(tid));
                if maybe_var_arenas.clone().any(|maybe_arena| maybe_arena.is_none()) {
                    continue 'rules;
                }
                let mut var_arenas_combo = ChunkCombo::new(maybe_var_arenas.map(Option::unwrap));
                'combo: while let Some(var_chunks) = var_arenas_combo.next() {
                    println!("Rule with var chunks: {:?}", var_chunks);
                    byte_buf.clear();

                    if !state_r.execute_ins_prepare_buf(
                        pos,
                        prev_pos,
                        &state_rule.rule_ins,
                        var_chunks,
                        byte_buf,
                    ) {
                        continue 'combo;
                    }

                    // prepare result, check novelty, and store
                    let res_tid = state_rule.res_info.tid;
                    let res_len = state_r
                        .type_map
                        .lookup_type_size(state_rule.res_info.tid)
                        .expect("IDK this type");
                    let result_chunk = {
                        let src_slice = match state_rule.res_info.res_src {
                            ResSrc::Const => state_r.const_bytes.as_slice(),
                            ResSrc::Buf => byte_buf.as_slice(),
                        };
                        let offset = state_rule.res_info.offset;
                        &src_slice[offset as usize..(offset as usize + res_len)]
                    };
                    if pos
                        .get(&res_tid)
                        .map(|arena| arena.contains(result_chunk).expect("wrong len"))
                        .unwrap_or(false)
                    {
                        println!("lol nevermind. already get this result");
                    } else {
                        println!("new result! {:?} {:?}", res_tid, result_chunk);
                        pos.entry(res_tid)
                            .or_insert_with(|| {
                                ChunkArena::new(
                                    state_r.type_map.lookup_type_size(res_tid).expect("idk fam"),
                                )
                            })
                            .insert(result_chunk)
                            .map(drop)
                            .expect("wahey");
                        continue 'all_rules;
                    }
                }
            }
            break 'all_rules;
        }
    }
}
impl std::fmt::Debug for PrintableAtom<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.tid {
            TypeId::U08 | TypeId::U32 => return self.chunk.fmt(f),
            _prod_tid => {}
        }

        let name = self.state.state_r.type_map.lookup_type_name(self.tid).expect("idk this name");

        let maybe_fields = self
            .state
            .state_r
            .type_map
            .lookup_maybe_fields(self.tid)
            .expect(&format!("idk your fields {:?}", self.tid));
        if let Some(fields) = maybe_fields {
            let mut d = f.debug_tuple(name);
            let d = &mut d;
            let mut offset = 0;
            for &field in fields {
                let field_size = self
                    .state
                    .state_r
                    .type_map
                    .lookup_type_size(field)
                    .expect("idk this field size");
                let offset2 = offset + field_size;
                let field_chunk = &self.chunk[offset..offset2];
                offset = offset2;
                d.field(&PrintableAtom { state: self.state, tid: field, chunk: field_chunk });
            }
            d.finish()
        } else {
            f.write_fmt(format_args!("{}", name))
        }
    }
}

impl std::fmt::Debug for PrintableStateInterpretation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let t = self.0.pos.iter().flat_map(|(&tid, arena)| {
            arena.iter().map(move |chunk| (PrintableAtom { state: &self.0, tid, chunk }, 'T'))
        });
        if let Some(prev_pos) = self.0.prev_pos.as_ref() {
            let u = prev_pos.iter().flat_map(|(tid, arena)| {
                arena
                    .iter()
                    .filter(|chunk| {
                        self.0
                            .pos
                            .get(tid)
                            .map(|pos_arena| !pos_arena.contains(chunk).expect("bad len"))
                            .unwrap_or(false)
                    })
                    .map(move |chunk| (PrintableAtom { state: &self.0, tid: *tid, chunk }, '?'))
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
    const PROCESSING: TypeId = TypeId(4);
    const RANK: TypeId = TypeId(5);
    const YES: TypeId = TypeId(6);
    const NO: TypeId = TypeId(7);
    const PERMITTED: TypeId = TypeId(8);
    let type_map = TypeMap::new(hashmap! {
        PROCESSING => ProdDef { name: "processing", maybe_fields: Some(vec![]), },
        RANK => ProdDef { name: "rank", maybe_fields: Some(vec![TypeId::U08]), },
        YES => ProdDef { name: "yes", maybe_fields: Some(vec![PROCESSING, RANK]), },
        NO => ProdDef { name: "no", maybe_fields: Some(vec![PROCESSING, RANK]), },
        PERMITTED => ProdDef { name: "permitted", maybe_fields: Some(vec![PROCESSING]), },
    })
    .expect("weh");
    println!("{:#?}", &type_map);
    let mut state = State {
        state_r: StateR {
            type_map,
            state_rules: vec![
                // yes(P,R1) :-  no(P,R1), yes(P,R2), R1<R2.
                StateRule {
                    var_types: vec![YES, NO],
                    rule_ins: vec![
                        RuleIns::CheckCmp {
                            frag_srcs: [
                                FragSrc { kind: FragSrcKind::Var { var_id: VarIdx(1) }, offset: 0 },
                                FragSrc { kind: FragSrcKind::Var { var_id: VarIdx(0) }, offset: 0 },
                            ],
                            len: 1,
                            op: CmpOp::Lt,
                        }, // wah
                        RuleIns::ExtendBuf {
                            src_range: 0..1,
                            extend_src: ExtendSrc::Var { var_id: VarIdx(1) },
                        }, // wah
                    ],
                    res_info: ResInfo { res_src: ResSrc::Buf, tid: YES, offset: 0 },
                },
                // no(P,R1) :-  yes(P,R1), no(P,R2), R1<R2.
                StateRule {
                    var_types: vec![NO, YES],
                    rule_ins: vec![
                        RuleIns::CheckCmp {
                            frag_srcs: [
                                FragSrc { kind: FragSrcKind::Var { var_id: VarIdx(1) }, offset: 0 },
                                FragSrc { kind: FragSrcKind::Var { var_id: VarIdx(0) }, offset: 0 },
                            ],
                            len: 1,
                            op: CmpOp::Lt,
                        }, // wah
                        RuleIns::ExtendBuf {
                            src_range: 0..1,
                            extend_src: ExtendSrc::Var { var_id: VarIdx(1) },
                        }, // wah
                    ],
                    res_info: ResInfo { res_src: ResSrc::Buf, tid: NO, offset: 0 },
                },
                // permitted(P) :- yes(P,R), ~no(P,R)
                StateRule {
                    var_types: vec![YES],
                    rule_ins: vec![
                        RuleIns::LitCheck {
                            sign: Sign::Neg,
                            tid: NO,
                            frag_src: FragSrc {
                                kind: FragSrcKind::Var { var_id: state::VarIdx(0) },
                                offset: 0,
                            },
                        }, // wah
                    ],
                    res_info: ResInfo { res_src: ResSrc::Buf, tid: PERMITTED, offset: 0 },
                },
                // no(processing(),rank(20)).
                StateRule {
                    var_types: vec![],
                    rule_ins: vec![],
                    res_info: ResInfo { res_src: ResSrc::Const, tid: NO, offset: 0 },
                },
                // yes(processing(),rank(30)).
                StateRule {
                    var_types: vec![],
                    rule_ins: vec![],
                    res_info: ResInfo { res_src: ResSrc::Const, tid: YES, offset: 1 },
                },
            ],
            const_bytes: vec![20, 30],
        },
        pos: Default::default(),
        prev_pos: None,
        prev_prev_pos: None,
    };
    println!("{:#?}", &state);

    let mut iterations = 0;
    loop {
        // println!("ADVANCED {:#?}", &kb);
        state.saturate();
        println!(
            "{} POS {:?}\nNEG {:?}\n$$$ {:?}\n",
            iterations,
            &state.pos,
            &state.prev_pos,
            PrintableStateInterpretation(&state),
        );
        // println!("SATURATED {:#?}", &kb);
        if iterations % 2 == 0 && state.pos_and_prev_prev_eq() {
            break;
        }
        state.advance();
        iterations += 1;
    }
    println!("iterations {:?}", iterations);
    println!("SOLUTION {:?}", PrintableStateInterpretation(&state));
    // state.saturate();
    println!("POS {:?}\nPREV_POS {:?}", &state.pos, &state.prev_pos);
    // state.saturate();
    // println!("{:#?}", &state.pos);
}
