use core::cmp::Ordering;
use core::ops::Range;
use maplit::hashmap;
use std::collections::HashMap;

mod chunk_arena;
mod old_kb;
mod program;
mod state;

use chunk_arena::{ChunkArena, ChunkCombo};

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

fn main() {
    state::test();
}
