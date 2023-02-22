use core::ops::Range;
use std::collections::HashMap;

mod chunk_arena;
mod program;

use chunk_arena::{ChunkArena, ChunkIter};

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

enum EqCheckKind {
    Eq,
    Neq,
}
struct VarIdx(u8);
struct EqCheck {
    kind: EqCheckKind,
    a: VarIdx,
    b: VarIdx,
    a_byte_offset: u16,
    b_byte_offset: u16,
    byte_len: u16,
}
struct ReturnInfo {
    built_range: Range<u16>,
    tid: TypeId,
}
struct StateRule {
    var_types: Vec<TypeId>, // will consider all quantifications
    eq_checks: Vec<EqCheck>,
    return_tid: TypeId,
    return_fields: VarIdx,
}
struct State {
    state_rules: Vec<StateRule>,
    pos: HashMap<TypeId, ChunkArena>,
}

////////////////////////////

impl State {
    fn generate(&mut self, rule: &StateRule) {
        let mut var_iters: Vec<ChunkIter> =
            rule.var_types.iter().map(|tid| self.pos.get(tid).unwrap().iter()).collect();
        if var_iters.iter().all(ChunkIter::has_next) {
            for eq_check in rule.eq_checks.iter() {
                // todo
            }
            // something something advance
            // build the return result
            // add it to the set
        }
    }
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

impl std::fmt::Debug for Valuation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let t = self.0.pos.iter().map(|x| (x, 'T'));
        if let Some(prev_pos) = self.0.prev_pos.as_ref() {
            let u = prev_pos.set_minus_iter(&self.0.pos).map(|atom| (atom, '?'));
            f.debug_map().entries(t.chain(u)).finish()
        } else {
            let u = None;
            f.debug_map().entries(t.chain(u)).finish()
        }
    }
}

impl std::fmt::Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl std::fmt::Debug for AtomSet {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_set().entries(self.sorted_vec.iter()).finish()
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
    let mut ca = chunk_arena::ChunkArena::new(3);
    println!("{:?}", &ca);
    println!("{:?}", ca.add(&[0, 1, 2]));
    println!("{:?}", &ca.data);
    println!("{:?}", ca.add(&[0, 1, 1]));
    println!("{:?}", &ca.data);
    println!("{:?}", ca.add(&[7, 1, 1]));
    println!("{:?}", &ca.data);
    println!("{:?}", ca.add(&[3, 1, 1]));
    println!("{:?}", &ca.data);
}

fn foo() {
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
