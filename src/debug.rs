use super::*;

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
