use core::cmp::Ordering;

#[derive(Debug)]
pub(crate) struct ChunkArena {
    pub(crate) data: Vec<u8>,
    chunk_bytes: usize,
}
pub(crate) struct ChunkIter<'a> {
    ca: &'a ChunkArena,
    next_index: usize,
}

////////////
impl ChunkIter<'_> {
    pub(crate) fn has_next(&self) -> bool {
        self.ca.get_index(self.next_index).is_some()
    }
}
impl<'a> Iterator for ChunkIter<'a> {
    type Item = &'a [u8];
    fn next(&mut self) -> Option<Self::Item> {
        let res = self.ca.get_index(self.next_index)?;
        self.next_index += 1;
        Some(res)
    }
}

impl ChunkArena {
    pub fn new(chunk_bytes: usize) -> Self {
        Self { data: Default::default(), chunk_bytes }
    }
    pub fn add(&mut self, chunk: &[u8]) -> Option<(&[u8], bool)> {
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
        Some((self.get_index(index).unwrap(), had))
    }

    fn check_chunk(&self, chunk: &[u8]) -> bool {
        chunk.len() <= self.chunk_bytes
    }
    fn len(&self) -> usize {
        self.data.len() / self.chunk_bytes
    }
    fn get_index(&self, index: usize) -> Option<&[u8]> {
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
            println!("lmr {:?}", [l, m, r]);
            match chunk_cmp(find, self.get_index(m).unwrap()) {
                Ordering::Equal => return (m, true),
                Ordering::Less => r = m,
                Ordering::Greater => l = m + 1,
            }
            println!("lmr' {:?}", [l, m, r]);
        }
        return (l, false);
    }
    pub fn iter(&self) -> ChunkIter {
        ChunkIter { ca: self, next_index: 0 }
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
