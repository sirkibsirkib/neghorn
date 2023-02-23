use core::cmp::Ordering;
use core::ops::Range;
use maplit::hashmap;
use std::collections::HashMap;

mod chunk_arena;
mod debug;
mod program;
mod saturate;

use chunk_arena::*;

#[derive(Default, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
struct Atom(&'static str);

#[derive(Default, Eq, PartialEq)]
struct AtomSet {
    sorted_vec: Vec<Atom>,
}

#[derive(Debug, Copy, Clone)]
struct Literal {
    atom: Atom,
    pos: bool,
}

#[derive(Debug)]
struct Rule {
    head: Atom,
    body: Vec<Literal>,
}

#[derive(Debug)]
struct Kb {
    pos: AtomSet,                   // inverse is interpretation #N
    prev_pos: Option<AtomSet>,      // inverse is interpretation #N-1
    prev_prev_pos: Option<AtomSet>, // // inverse is interpretation #N-2
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct TypeId(u32);
struct TypeInfo {
    type_fields: HashMap<TypeId, Vec<TypeId>>,
    type_size: HashMap<TypeId, usize>, // sum of all
}

struct Valuation<'a>(&'a Kb);

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
}

trait WordRange {
    fn word_range(&self) -> Range<usize>;
}
////////////////////////////

impl WordRange for Range<u16> {
    #[inline(always)]
    fn word_range(&self) -> Range<usize> {
        self.start as usize..self.end as usize
    }
}

fn chunk_cmp(a: &[u8], b: &[u8]) -> Ordering {
    for (a, b) in a.iter().zip(b.iter()) {
        match a.cmp(b) {
            Ordering::Equal => {}
            o => return o,
        }
    }
    Ordering::Equal
}

impl TypeInfo {
    const INT_TID: TypeId = TypeId(0);
    fn compute_size_of(&self, tid: TypeId, depth_to_go: u8) -> Result<usize, TidSizeErr> {
        match tid {
            Self::INT_TID => Ok(std::mem::size_of::<u32>()),
            _ => {
                if depth_to_go == 0 {
                    Err(TidSizeErr::RecursiveDepthExceeded)
                } else {
                    let fields = self
                        .type_fields
                        .get(&tid)
                        .ok_or(TidSizeErr::DependsOnSizeOfUnknown(tid))?;
                    fields.iter().fold(Ok(0), |acc, &field| {
                        Ok(acc? + self.compute_size_of(field, depth_to_go - 1)?)
                    })
                }
            }
        }
    }
}

impl Literal {
    fn pos(atom: Atom) -> Self {
        Self { pos: true, atom }
    }
    fn neg(atom: Atom) -> Self {
        Self { pos: false, atom }
    }
}

impl AtomSet {
    fn iter(&self) -> impl Iterator<Item = Atom> + '_ {
        self.sorted_vec.iter().copied()
    }
    fn insert(&mut self, atom: Atom) -> bool {
        match self.sorted_vec.binary_search(&atom) {
            Ok(_) => false,
            Err(i) => {
                self.sorted_vec.insert(i, atom);
                true
            }
        }
    }
    fn contains(&self, atom: Atom) -> bool {
        self.sorted_vec.binary_search(&atom).is_ok()
    }
    fn set_minus_iter<'a, 'b: 'a>(&'a self, other: &'b Self) -> impl Iterator<Item = Atom> + 'a {
        self.sorted_vec.iter().copied().filter(|&atom| !other.contains(atom))
    }
}

impl Default for Kb {
    fn default() -> Self {
        Self { pos: Default::default(), prev_pos: None, prev_prev_pos: None }
    }
}
impl Kb {
    fn contains(&self, lit: Literal) -> bool {
        if lit.pos {
            self.pos.contains(lit.atom)
        } else {
            if let Some(prev_pos) = &self.prev_pos {
                !prev_pos.contains(lit.atom)
            } else {
                false
            }
        }
    }
}
impl Rule {
    fn body_sat(&self, kb: &Kb) -> bool {
        self.body.iter().all(|&subgoal| kb.contains(subgoal))
    }
}

impl Kb {
    fn advance(&mut self) {
        self.prev_prev_pos = self.prev_pos.replace(std::mem::take(&mut self.pos));
    }
    fn saturate(&mut self, rules: &[Rule]) {
        'c: loop {
            // println!("pos:{:?}", &self.pos);
            for rule in rules {
                if !self.contains(Literal::pos(rule.head)) && rule.body_sat(self) {
                    // println!("applicable {:?}", rule);
                    self.pos.insert(rule.head);
                    continue 'c;
                }
            }
            break 'c;
        }
    }
}

fn main() {
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
    println!("{:#?}", &state.pos);
    state.generate_all();
    println!("{:#?}", &state.pos);
    state.generate_all();
    println!("{:#?}", &state.pos);
}

fn main2() {
    let mut ca = chunk_arena::ChunkArena::new(3);
    println!("{:?}", &ca);
    println!("{:?}", ca.insert(&[0, 1, 2]));
    println!("{:?}", &ca.data);
    println!("{:?}", ca.insert(&[0, 1, 1]));
    println!("{:?}", &ca.data);
    println!("{:?}", ca.insert(&[7, 1, 1]));
    println!("{:?}", &ca.data);
    println!("{:?}", ca.insert(&[3, 1, 1]));
    println!("{:?}", &ca.data);
}

fn main1() {
    let rules = vec![
        Rule {
            head: Atom("other_role_in(amy,r1,a)"),
            body: vec![Literal::pos(Atom("role_in(bob,r1,a)"))],
        }, //whee
        Rule {
            head: Atom("role_in(amy,r1,a)"),
            body: vec![
                Literal::pos(Atom("role_in(amy,r2,a)")),
                Literal::neg(Atom("other_role_in(amy,r1,a)")),
            ],
        }, //whee
        Rule { head: Atom("role_in(amy,r2,a)"), body: vec![] }, //whee
        Rule { head: Atom("role_in(bob,r1,a)"), body: vec![] }, //whee
    ];

    // //
    // let rules = vec![
    //     Rule { head: Atom('T'), body: vec![Literal::pos(Atom('t'))] }, //whee
    //     Rule { head: Atom('V'), body: vec![Literal::pos(Atom('h')), Literal::neg(Atom('T'))] },
    //     Rule { head: Atom('h'), body: vec![] },
    //     // Rule { head: Atom('t'), body: vec![] },
    // ];

    let mut kb = Kb { pos: Default::default(), prev_pos: None, prev_prev_pos: None };
    let start = std::time::Instant::now();
    let mut iterations_taken = 0;
    for i in 0.. {
        // println!("ADVANCED {:#?}", &kb);
        kb.saturate(&rules);
        // println!("SATURATED {:#?}", &kb);
        if i % 2 == 0 && Some(&kb.pos) == kb.prev_prev_pos.as_ref() {
            iterations_taken = i;
            break;
        }
        kb.advance();
    }
    println!("Time taken, start to finish: {:?}", start.elapsed());
    println!("iterations_taken: {:?}", iterations_taken);
    println!("{:#?}", Valuation(&kb));
}
