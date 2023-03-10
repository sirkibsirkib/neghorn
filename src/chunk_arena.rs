use crate::chunk_cmp;
use core::cmp::Ordering;

#[derive(Eq)]
pub(crate) struct ChunkArena {
    data: Vec<u8>,
    chunk_bytes: usize,
    zero_len_and_full: bool,
}

pub(crate) struct ChunkIter<'a> {
    arena: &'a ChunkArena,
    next_chunk_index: usize,
}

#[derive(Debug)]
pub(crate) struct ChunkCombo<'a> {
    // slots.next_chunk_index trails behind chunks if they exist
    slots: Vec<ChunkSlot<'a>>,
    chunks: Vec<&'a [u8]>,
    ever_returned: bool,
}
#[derive(Debug)]
struct ChunkSlot<'a> {
    next_chunk_index: usize, // next chunk to read to fill ChunkCombo::chunks
    arena: &'a ChunkArena,
}
#[derive(Debug, Copy, Clone)]
pub(crate) struct WrongSize {
    given: usize,
    expected: usize,
}
////////////

impl std::fmt::Debug for ChunkArena {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<'a> Iterator for ChunkIter<'a> {
    type Item = &'a [u8];
    fn next(&mut self) -> Option<Self::Item> {
        let res = self.arena.get_index(self.next_chunk_index)?;
        self.next_chunk_index += 1;
        Some(res)
    }
}
impl<'a> ChunkCombo<'a> {
    pub(crate) fn new(arenas: impl Iterator<Item = &'a ChunkArena>) -> Self {
        Self {
            ever_returned: false,
            chunks: Default::default(),
            slots: arenas.map(|arena| ChunkSlot { next_chunk_index: 0, arena }).collect(),
        }
    }
    fn try_fill_chunks(&mut self) {
        'outer: loop {
            // BEFORE: slot_next_indices:[X,Y]   chunks:[chunk(arena0,X-1)]
            // AFTER : slot_next_indices:[X,Y+1] chunks:[chunk(arena0,X-1),chunk(arena1,Y)]
            for (i, slot) in self.slots[self.chunks.len()..].iter_mut().enumerate() {
                if let Some(chunk) = slot.arena.get_index(slot.next_chunk_index) {
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
                        return;
                    }
                    continue 'outer;
                }
            }
            assert_eq!(self.chunks.len(), self.slots.len());
            return; // ok! ch
        }
    }
}
impl<'a> ChunkCombo<'a> {
    pub(crate) fn next(&mut self) -> Option<&[&[u8]]> {
        self.chunks.pop();
        // advance to the next combination by incrmenting last slot by 1.
        // BEFORE: slot_next_indices:[X,Y] chunks:[chunk(arena0,X-1),chunk(arena1,Y-1)]
        // AFTER : slot_next_indices:[X,Y] chunks:[chunk(arena0,X-1)]
        self.try_fill_chunks();
        if self.chunks.is_empty() && self.ever_returned {
            return None;
        }

        // slot_next_indices:[X,Y] chunks:[chunk(arena0,X-1),chunk(arena1,Y-1)]
        self.ever_returned = true;
        Some(&self.chunks)
    }
}

impl PartialEq for ChunkArena {
    fn eq(&self, other: &Self) -> bool {
        self.chunk_bytes == other.chunk_bytes // assumed true for all my cases
        && self.data == other.data
    }
}
impl ChunkArena {
    pub fn iter(&self) -> ChunkIter {
        ChunkIter { arena: self, next_chunk_index: 0 }
    }
    pub fn from_slice<'a, const N: usize>(i: impl Iterator<Item = &'a [u8; N]> + 'a) -> Self {
        let mut me = Self::new(N);
        for chunk in i {
            me.insert(chunk).expect("N constant, no?");
        }
        me
    }
    pub fn new(chunk_bytes: usize) -> Self {
        Self { data: Default::default(), chunk_bytes, zero_len_and_full: false }
    }
    pub fn contains(&self, chunk: &[u8]) -> Result<bool, WrongSize> {
        self.check_chunk_len(chunk)?;
        if self.chunk_bytes == 0 {
            return Ok(self.zero_len_and_full);
        }
        Ok(self.binary_search(chunk).1)
    }
    pub fn insert(&mut self, chunk: &[u8]) -> Result<(&[u8], bool), WrongSize> {
        self.check_chunk_len(chunk)?;
        if self.chunk_bytes == 0 {
            let ret = !self.zero_len_and_full;
            self.zero_len_and_full = true;
            return Ok((&[], ret));
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
        Ok((self.get_index(index).unwrap(), had))
    }

    fn check_chunk_len(&self, chunk: &[u8]) -> Result<(), WrongSize> {
        if self.chunk_bytes != chunk.len() {
            return Err(WrongSize { given: chunk.len(), expected: self.chunk_bytes });
        } else {
            Ok(())
        }
    }
    fn len(&self) -> usize {
        self.data.len() / self.chunk_bytes
    }
    // fn contains(&self, chunk)
    fn get_index(&self, index: usize) -> Option<&[u8]> {
        if self.chunk_bytes == 0 {
            return if self.zero_len_and_full && index == 0 { Some(&[]) } else { None };
        }
        let start = self.chunk_bytes * index;
        let range = start..(start + self.chunk_bytes);
        if self.data.len() < range.end {
            None
        } else {
            Some(&self.data[range])
        }
    }
    fn binary_search(&self, find: &[u8]) -> (usize, bool) {
        // NOT DESIGNED FOR self.chunk_bytes == 0
        if self.data.is_empty() {
            return (0, false);
        }
        let mut l = 0;
        let mut r = self.len();
        while l < r {
            let m = (l + r) / 2;
            match chunk_cmp(find, self.get_index(m).unwrap()) {
                Ordering::Equal => return (m, true),
                Ordering::Less => r = m,
                Ordering::Greater => l = m + 1,
            }
        }
        return (l, false);
    }
}

pub(crate) fn test() {
    let mut ca = ChunkArena::new(3);
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

pub(crate) fn zero_len_test() {
    let mut ca = ChunkArena::from_slice::<0>([].iter());
    println!("{:?}", &ca);
    ca.insert(&[]).expect("ok");
    println!("{:?}", &ca);
    ca.insert(&[]).expect("ok");
    println!("{:?}", &ca);
}
