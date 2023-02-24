use std::collections::HashSet;

use std::collections::HashMap;

struct Variable(String);

enum Subgoal {
    Eq { pos: bool, a: Atom, b: Atom },
    Literal { pos: bool, a: Atom },
}

enum Atom {
    Variable(Variable),
    Function { func: String, args: Vec<Atom> },
}

struct Rule {
    head: Atom,
    body: HashSet<Subgoal>,
}

struct Program {
    types_params: HashMap<String, Option<Vec<String>>>,
    emits: HashSet<String>,
    seals: HashSet<String>,
    rules: Vec<Rule>,
}
