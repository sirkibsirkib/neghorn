use core::cmp::Ordering;
use core::ops::Range;
use maplit::hashmap;
use std::collections::HashMap;

mod chunk_arena;
mod old_kb;
mod program;
mod state;

use chunk_arena::{ChunkArena, ChunkCombo};

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
struct PrintableStateInterpretation<'a>(&'a State);

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

fn main() {
    state::test();
}
