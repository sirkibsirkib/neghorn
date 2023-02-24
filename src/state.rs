use super::*;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct TypeId(u32);

#[derive(Debug)]
struct TypeDef {
    name: &'static str,
    fields: Vec<TypeId>,
}

struct ConstTypeDef {
    name: &'static str,
    size: usize,
}

#[derive(Debug)]
struct TypeInfo {
    type_defs: HashMap<TypeId, TypeDef>,
    type_size: HashMap<TypeId, usize>, // sum of all
    closed_types: HashSet<TypeId>,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum TidSizeErr {
    RecursiveDepthExceeded,
    DependsOnSizeOfUnknown(TypeId),
}

#[derive(Debug, Copy, Clone)]
enum CmpOp {
    Leq,
    Lt,
    Eq,
    Neq,
}
#[derive(Debug, Clone, Copy)]
struct VarIdx(u8);

struct ResInfo {
    tid: TypeId,
    res_src: ResSrc,
    offset: u16,
}
enum ResSrc {
    Const,
    Buf,
}
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
    ExtendBuf { src_range: Range<u16>, src: ExtendSrc },
    Truncate { new_len: u16 },
    CheckCmp { frag_srcs: [FragSrc; 2], len: u16, op: CmpOp },
    LitCheck { sign: Sign, frag_src: FragSrc, tid: TypeId },
}
struct StateR {
    state_rules: Vec<StateRule>,
    const_bytes: Vec<u8>,
    type_info: TypeInfo,
}
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
impl std::fmt::Debug for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_fmt(format_args!("tid{}", self.0))
    }
}

impl TypeId {
    const U08: Self = Self(0);
    const U32: Self = Self(1);
    const TAG: Self = Self(8);
    const fn const_def(self) -> Option<ConstTypeDef> {
        Some(match self {
            Self::U08 => ConstTypeDef { size: 1, name: "u08" },
            Self::U32 => ConstTypeDef { size: 4, name: "u32" },
            Self::TAG => ConstTypeDef { size: 8, name: "tag" },
            _ => return None,
        })
    }
}
impl ConstTypeDef {
    fn size(self) -> usize {
        self.size
    }
    fn name(self) -> &'static str {
        self.name
    }
}
impl TypeInfo {
    fn lookup_type_size(&self, tid: TypeId) -> Result<usize, ()> {
        tid.const_def()
            .map(ConstTypeDef::size)
            .or_else(|| self.type_size.get(&tid).copied())
            .ok_or(())
    }
    fn lookup_type_fields(&self, tid: TypeId) -> Result<Option<&[TypeId]>, ()> {
        if tid.const_def().is_some() {
            Ok(None)
        } else {
            self.type_defs.get(&tid).map(|def| Some(def.fields.as_slice())).ok_or(())
        }
    }
    fn lookup_type_name(&self, tid: TypeId) -> Result<&'static str, ()> {
        tid.const_def()
            .map(ConstTypeDef::name)
            .or_else(|| self.type_defs.get(&tid).map(|def| def.name))
            .ok_or(())
    }
    fn new(type_defs: HashMap<TypeId, TypeDef>, closed_types: HashSet<TypeId>) -> Result<Self, ()> {
        let mut size_todo: HashSet<TypeId> = type_defs.keys().copied().collect();
        let mut type_size: HashMap<TypeId, usize> = Default::default();

        // invariant: type_size.keys() disjoint with size_todo.
        const MAX_DEPTH: u8 = 100;

        'arb: while let Some(mut tid) = size_todo.iter().copied().next() {
            'tid_compute: for _ in 0..MAX_DEPTH {
                assert!(tid.const_def().is_none());
                let def = type_defs.get(&tid).ok_or(())?;
                let r: Result<usize, TypeId> = def.fields.iter().fold(Ok(0), |acc, field| {
                    Ok(acc?
                        + field
                            .const_def()
                            .map(ConstTypeDef::size)
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
        Ok(Self { type_defs, type_size, closed_types })
    }
}

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
                RuleIns::ExtendBuf { src, src_range } => {
                    let src_slice = match src {
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
                    let len = self.type_info.lookup_type_size(tid).expect("IDK this type");
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
                ChunkArena::new(state_r.type_info.type_size.get(&tid).copied().expect("idk fam"))
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
                    let res_len = state_r
                        .type_info
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
                        .get(&state_rule.res_info.tid)
                        .map(|arena| arena.contains(result_chunk).expect("wrong len"))
                        .unwrap_or(false)
                    {
                        println!("lol nevermind. already get this result");
                    } else {
                        println!("new result! {:?} {:?}", state_rule.res_info.tid, result_chunk);
                        Self::add_pos(state_r, pos, state_rule.res_info.tid, result_chunk)
                            .expect("wrong size??");
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
        let maybe_fields = self
            .state
            .state_r
            .type_info
            .lookup_type_fields(self.tid)
            .expect(&format!("idk your fields {:?}", self.tid));
        if let Some(fields) = maybe_fields {
            let name =
                self.state.state_r.type_info.lookup_type_name(self.tid).expect("idk this name");
            let mut d = f.debug_tuple(name);
            let d = &mut d;
            let mut offset = 0;
            for &field in fields {
                let field_size = self
                    .state
                    .state_r
                    .type_info
                    .lookup_type_size(field)
                    .expect("idk this field size");
                let offset2 = offset + field_size;
                let field_chunk = &self.chunk[offset..offset2];
                offset = offset2;
                d.field(&PrintableAtom { state: self.state, tid: field, chunk: field_chunk });
            }
            d.finish()
        } else {
            match self.tid {
                TypeId::U08 | TypeId::U32 => self.chunk.fmt(f),
                TypeId::TAG => f.write_fmt(format_args!(
                    "{}",
                    std::str::from_utf8(self.chunk).map(str::trim).unwrap_or("?")
                )),
                _ => unreachable!(),
            }
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
    let type_info = TypeInfo::new(
        hashmap! {
            TypeId(5) => TypeDef { name: "person", fields: vec![TypeId::U08], },
            TypeId(6) => TypeDef { name: "friend", fields: vec![TypeId(5), TypeId(5)], },
            TypeId(7) => TypeDef { name: "cool-person", fields: vec![TypeId(5)] },
        },
        hashset! {},
    )
    .expect("weh");
    println!("{:#?}", &type_info);
    // return;
    let mut state = State {
        state_r: StateR {
            type_info,
            state_rules: vec![
                // person(0) :- person(2), !person(1).
                StateRule {
                    var_types: vec![],
                    rule_ins: vec![
                        RuleIns::LitCheck {
                            tid: TypeId(5),
                            sign: Sign::Pos,
                            frag_src: FragSrc { offset: 2, kind: FragSrcKind::Const },
                        },
                        RuleIns::LitCheck {
                            tid: TypeId(5),
                            sign: Sign::Neg,
                            frag_src: FragSrc { offset: 1, kind: FragSrcKind::Const },
                        },
                    ],
                    res_info: ResInfo { res_src: ResSrc::Const, tid: TypeId(5), offset: 0 },
                },
                // person(1) :- !person(0).
                StateRule {
                    var_types: vec![],
                    rule_ins: vec![RuleIns::LitCheck {
                        tid: TypeId(5),
                        sign: Sign::Neg,
                        frag_src: FragSrc { offset: 0, kind: FragSrcKind::Const },
                    }],
                    res_info: ResInfo { res_src: ResSrc::Const, tid: TypeId(5), offset: 1 },
                },
                // person(2).
                StateRule {
                    var_types: vec![],
                    rule_ins: vec![],
                    res_info: ResInfo { res_src: ResSrc::Const, tid: TypeId(5), offset: 2 },
                },
            ],
            const_bytes: vec![0, 1, 2],
        },
        pos: Default::default(),
        prev_pos: None,
        prev_prev_pos: None,
    };
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
