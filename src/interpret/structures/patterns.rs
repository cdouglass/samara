use std::collections::HashMap;

use interpret::structures::Atom;
use interpret::structures::Term;

pub enum Pattern {
    Sum(usize, String, Box<Pattern>),
    Atom(Atom),
    Var(usize, String)
}

impl Pattern {
    fn match_term(&self, term: &Term) -> Option<HashMap<usize, Term>> {
        match (self, term) {
            (&Pattern::Sum(ref n, _, ref pat), &Term::Sum(ref m, _, ref val)) => {
                if m == n {
                    pat.match_term(val)
                } else { None }
            },
            (&Pattern::Atom(ref a), &Term::Atom(ref b)) => {
                if a == b {
                    Some(HashMap::new())
                } else { None }
            },
            (&Pattern::Var(ref n, _), _) => {
                let mut sub = HashMap::new();
                sub.insert(*n, term.clone());
                Some(sub)
            },
            _ => None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use self::Pattern::*;
    use interpret::structures::Atom;
    use interpret::structures::Op;
    use interpret::structures::Term;

    const JUST : usize = 0;
    const NONE : usize = 1;
    const LEFT : usize = 2;
    const RIGHT : usize = 3;

    fn single_sub(n: usize, value: &Term) -> HashMap<usize, Term> {
        let mut sub = HashMap::new();
        sub.insert(n, value.clone());
        sub
    }

    #[test]
    fn test_atom_pattern() {
        let pat = Atom(Atom::Int(5));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(5))), Some(HashMap::new()));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(true))), None);

        let pat = Atom(Atom::Bool(true));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(true))), Some(HashMap::new()));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(false))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);

        let pat = Atom(Atom::Unit);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Unit)), Some(HashMap::new()));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(false))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);

        let pat = Atom(Atom::BuiltIn(Op::Mul));
        assert_eq!(pat.match_term(&Term::Atom(Atom::BuiltIn(Op::Mul))), Some(HashMap::new()));
        assert_eq!(pat.match_term(&Term::Atom(Atom::BuiltIn(Op::Add))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);
    }

    #[test]
    fn test_irrefutable_pattern() {
        let irrefutable = Var(1, String::from("x"));
        let five = Term::Atom(Atom::Int(5));
        let square = Term::Lambda(Box::new(Term::App(Box::new(Term::App(Box::new(Term::Atom(Atom::BuiltIn(Op::Mul))), Box::new(Term::Var(0, String::from("y"))))), Box::new(Term::Var(0, String::from("y"))))), String::from("square"));
        let none = Term::Sum(NONE, String::from("None"), Box::new(Term::Atom(Atom::Unit)));
        let unit = Term::Atom(Atom::Unit);

        assert_eq!(irrefutable.match_term(&five), Some(single_sub(1, &five)));
        assert_eq!(irrefutable.match_term(&square), Some(single_sub(1, &square)));
        assert_eq!(irrefutable.match_term(&none), Some(single_sub(1, &none)));
        assert_eq!(irrefutable.match_term(&unit), Some(single_sub(1, &unit)));
    }

    #[test]
    fn test_sum_pattern() {
        let irrefutable = Var(1, String::from("x"));
        let pat = Sum(LEFT, String::from("Left"), Box::new(irrefutable));
        let square = Term::Lambda(Box::new(Term::App(Box::new(Term::App(Box::new(Term::Atom(Atom::BuiltIn(Op::Mul))), Box::new(Term::Var(0, String::from("y"))))), Box::new(Term::Var(0, String::from("y"))))), String::from("square"));

        let mut expected = HashMap::new();
        expected.insert(1, square.clone());
        let actual = pat.match_term(&Term::Sum(LEFT, String::from("Left"), Box::new(square.clone())));
        assert_eq!(actual, Some(expected));

        assert_eq!(pat.match_term(&Term::Sum(RIGHT, String::from("Right"), Box::new(square.clone()))), None);
        assert_eq!(pat.match_term(&Term::Constructor(RIGHT, String::from("Right"))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(5))), None);
    }
}
