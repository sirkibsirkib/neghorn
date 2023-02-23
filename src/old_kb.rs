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

fn test() {
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
