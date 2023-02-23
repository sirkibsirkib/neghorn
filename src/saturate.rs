use super::*;

impl VarFrag {
    fn get_slice<'a>(self, var_chunks: &'a [&'a [u8]]) -> &'a [u8] {
        // assumes in bounds!
        &var_chunks[self.var_idx.0 as usize]
            [self.var_bytes.start as usize..self.var_bytes.end as usize]
    }
}
impl State {
    fn frag_slice<'a, 'b>(&'a self, frag: &'b Frag, var_chunks: &'a [&'a [u8]]) -> &'a [u8] {
        &(match frag.source {
            FragSource::Var(vid) => &var_chunks[vid.0 as usize],
            FragSource::Const => self.const_bytes.as_slice(),
        }[frag.bytes_range.word_range()])
    }
    pub(crate) fn generate_all(&mut self) {
        let mut result_buf: Vec<u8> = vec![];
        'rules: for state_rule in self.state_rules.iter() {
            let mut var_arenas_combo =
                ChunkCombo::new(state_rule.var_types.iter().map(|tid| self.pos.get(tid).unwrap()));
            'combo: while let Some(var_chunks) = var_arenas_combo.next() {
                // eq checks
                for cmp_check in state_rule.frag_cmp_checks.iter() {
                    let slice_a = self.frag_slice(&cmp_check.frag_a, var_chunks);
                    let slice_b = self.frag_slice(&cmp_check.frag_b, var_chunks);
                    let pass = match cmp_check.cmp_kind {
                        CmpKind::Eq => slice_a == slice_b,
                        CmpKind::Neq => slice_a != slice_b,
                        CmpKind::Lt => chunk_cmp(slice_a, slice_b) == Ordering::Less,
                        CmpKind::Leq => chunk_cmp(slice_a, slice_b) != Ordering::Greater,
                    };
                    if !pass {
                        continue 'combo;
                    }
                }
                // lit tests
                result_buf.clear();
                for frag in state_rule.result_frags.iter() {
                    result_buf.extend(self.frag_slice(frag, var_chunks));
                }
                if self
                    .pos
                    .get(&state_rule.result_tid)
                    .expect("unknown TID")
                    .contains(&result_buf)
                    .expect("wrong len")
                {
                    println!("lol nevermind");
                } else {
                    println!("new result!");
                    self.pos.get_mut(&state_rule.result_tid).unwrap().insert(&result_buf).unwrap();
                    continue 'rules;
                }
            }
        }
    }
}
