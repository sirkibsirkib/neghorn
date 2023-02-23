use core::cmp::Ordering;

#[derive(Debug)]
pub(crate) struct ChunkArena {
    pub(crate) data: Vec<u8>,
    chunk_bytes: usize,
}
// pub(crate) struct ChunkIter<'a> {
//     ca: &'a ChunkArena,
//     next_index: usize,
// }

////////////
#[derive(Debug)]
struct ChunkSlot<'a> {
    next_chunk_index: usize, // next chunk to read to fill ChunkCombo::chunks
    arena: &'a ChunkArena,
}
#[derive(Debug)]
pub(crate) struct ChunkCombo<'a> {
    // invariant: indices in range
    slots: Vec<ChunkSlot<'a>>,
    chunks: Vec<&'a [u8]>,
}
impl<'a> ChunkCombo<'a> {
    pub(crate) fn new(arenas: impl Iterator<Item = &'a ChunkArena>) -> Self {
        Self {
            chunks: Default::default(),
            slots: arenas.map(|arena| ChunkSlot { next_chunk_index: 0, arena }).collect(),
        }
    }
}
impl<'a> ChunkCombo<'a> {
    pub(crate) fn next(&mut self) -> Option<&[&[u8]]> {
        // chunks are either EMPTY (first iteration) or FULL (otherwise)

        // advance to the next combination by incrmenting last slot by 1.
        // BEFORE: slot_next_indices:[X,Y] chunks:[chunk(arena0,X-1),chunk(arena1,Y-1)]
        // AFTER : slot_next_indices:[X,Y] chunks:[chunk(arena0,X-1)]
        self.chunks.pop(); // no effect if EMPTY already :) perfect.
        'outer: loop {
            if self.slots.is_empty() {
                // that's the signal for being done!
                return None;
            }

            // BEFORE: slot_next_indices:[X,Y]   chunks:[chunk(arena0,X-1)]
            // AFTER : slot_next_indices:[X,Y+1] chunks:[chunk(arena0,X-1),chunk(arena1,Y)]
            for (i, slot) in self.slots[self.chunks.len()..].iter_mut().enumerate() {
                if let Some(chunk) = slot.arena.get(slot.next_chunk_index) {
                    self.chunks.push(chunk);
                    slot.next_chunk_index += 1;
                } else {
                    // back up!
                    // BEFORE: slot_next_indices:[X,Y] chunks:[chunk(arena0,X-1)]
                    // AFTER : slot_next_indices:[X,0] chunks:[chunk(arena0,X-1)]
                    for slot in self.slots[(self.chunks.len() + i)..].iter_mut() {
                        slot.next_chunk_index = 0;
                    }
                    // BEFORE: slot_next_indices:[X,0] chunks:[chunk(arena0,X-1)]
                    // AFTER : slot_next_indices:[X,0] chunks:[]
                    if self.chunks.pop().is_none() {
                        // case of chunks==[] before! (trying to set chunk len to -1).
                        // instead, mark STOP condition: slots := chunks := [].
                        self.slots.clear();
                        self.chunks.clear();
                    }
                    continue 'outer;
                }
            }
            break 'outer; // ok!
        }
        // slot_next_indices:[X,Y] chunks:[chunk(arena0,X-1),chunk(arena1,Y-1)]
        Some(&self.chunks)
    }
}

impl ChunkArena {
    pub fn from_slice<'a, const N: usize>(i: impl Iterator<Item = &'a [u8; N]> + 'a) -> Self {
        let mut me = Self::new(N);
        for chunk in i {
            me.insert(chunk);
        }
        me
    }
    pub fn new(chunk_bytes: usize) -> Self {
        Self { data: Default::default(), chunk_bytes }
    }
    pub fn insert(&mut self, chunk: &[u8]) -> Option<(&[u8], bool)> {
        if !self.check_chunk(chunk) {
            return None;
        }
        let (index, had) = self.binary_search(chunk);
        if !had {
            // insert!
            self.data.reserve(self.chunk_bytes);
            let offset = self.chunk_bytes * index;
            let bytes_to_move = self.data.len() - offset;
            unsafe {
                // shift everything over
                let start = self.data.as_mut_ptr().add(offset);
                std::ptr::copy(start, start.add(self.chunk_bytes), bytes_to_move);
                // write in
                std::ptr::copy_nonoverlapping(chunk.as_ptr(), start, self.chunk_bytes);
                self.data.set_len(self.data.len() + self.chunk_bytes);
            }
        }
        Some((self.get(index).unwrap(), had))
    }

    fn check_chunk(&self, chunk: &[u8]) -> bool {
        chunk.len() <= self.chunk_bytes
    }
    fn len(&self) -> usize {
        self.data.len() / self.chunk_bytes
    }
    fn get(&self, index: usize) -> Option<&[u8]> {
        let start = self.chunk_bytes * index;
        let range = start..(start + self.chunk_bytes);
        if self.data.len() < range.end {
            None
        } else {
            Some(&self.data[range])
        }
    }
    fn binary_search(&self, find: &[u8]) -> (usize, bool) {
        if self.data.is_empty() {
            return (0, false);
        }
        let mut l = 0;
        let mut r = self.len();
        while l < r {
            let m = (l + r) / 2;
            match chunk_cmp(find, self.get(m).unwrap()) {
                Ordering::Equal => return (m, true),
                Ordering::Less => r = m,
                Ordering::Greater => l = m + 1,
            }
        }
        return (l, false);
    }
    // pub fn iter(&self) -> ChunkIter {
    //     ChunkIter { ca: self, next_index: 0 }
    // }
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
