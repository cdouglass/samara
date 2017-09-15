use interpret::structures::Atom;
use interpret::structures::Term;

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub enum Pattern {
    Wildcard,
    Sum(usize, String, Vec<Pattern>),
    Atom(Atom),
    Var(String)
}

impl Pattern {
    pub fn match_term(&self, term: &Term) -> Option<Vec<Term>> {
        match (self, term) {
            (&Pattern::Wildcard, _) => Some(vec![]),
            (&Pattern::Sum(ref n, _, ref patterns), &Term::Sum(ref m, _, ref values)) => {
                if m == n {
                    let mut matches = vec![];
                    for (pat, val) in patterns.iter().zip(values.iter()) {
                        match pat.match_term(val) {
                            Some(ms) => { matches.extend(ms); },
                            None => { return None; }
                        }
                    }
                    Some(matches)
                } else {
                    None
                }
            },
            (&Pattern::Atom(ref a), &Term::Atom(ref b)) => {
                if a == b {
                    Some(vec![])
                } else { None }
            },
            (&Pattern::Var(_), _) => { Some(vec![term.clone()]) },
            _ => None
        }
    }

    pub fn identifiers(&self) -> Vec<&String> {
        match *self {
            Pattern::Var(ref s) => vec![s],
            Pattern::Sum(_, _, ref patterns) => {
                patterns.iter().fold(vec![], (|mut v, pat| {
                    v.extend(pat.identifiers());
                    v}))
            }
            _ => vec![]
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

    #[test]
    fn test_wildcard_pattern() {
        let pat = Wildcard;
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(5))), Some(vec![]));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), Some(vec![]));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(true))), Some(vec![]));
    }

    #[test]
    fn test_atom_pattern() {
        let pat = Atom(Atom::Int(5));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(5))), Some(vec![]));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(true))), None);

        let pat = Atom(Atom::Bool(true));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(true))), Some(vec![]));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(false))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);

        let pat = Atom(Atom::Unit);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Unit)), Some(vec![]));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(false))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);

        let pat = Atom(Atom::BuiltIn(Op::Mul));
        assert_eq!(pat.match_term(&Term::Atom(Atom::BuiltIn(Op::Mul))), Some(vec![]));
        assert_eq!(pat.match_term(&Term::Atom(Atom::BuiltIn(Op::Add))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);
    }

    #[test]
    fn test_irrefutable_pattern() {
        let irrefutable = Var(String::from("x"));
        let five = Term::Atom(Atom::Int(5));
        let square = Term::Lambda(Box::new(Term::App(Box::new(Term::App(Box::new(Term::Atom(Atom::BuiltIn(Op::Mul))), Box::new(Term::Var(0, String::from("y"))))), Box::new(Term::Var(0, String::from("y"))))), String::from("square"));
        let none = Term::Sum(NONE, String::from("None"), vec![]);
        let unit = Term::Atom(Atom::Unit);

        assert_eq!(irrefutable.match_term(&five), Some(vec![five]));
        assert_eq!(irrefutable.match_term(&square), Some(vec![square]));
        assert_eq!(irrefutable.match_term(&none), Some(vec![none]));
        assert_eq!(irrefutable.match_term(&unit), Some(vec![unit]));
    }

    #[test]
    fn test_sum_pattern() {
        let irrefutable = Var(String::from("x"));
        let pat = Sum(LEFT, String::from("Left"), vec![irrefutable]);
        let square = Term::Lambda(Box::new(Term::App(Box::new(Term::App(Box::new(Term::Atom(Atom::BuiltIn(Op::Mul))), Box::new(Term::Var(0, String::from("y"))))), Box::new(Term::Var(0, String::from("y"))))), String::from("square"));

        let expected = vec![square.clone()];
        let actual = pat.match_term(&Term::Sum(LEFT, String::from("Left"), vec![square.clone()]));
        assert_eq!(actual, Some(expected));

        assert_eq!(pat.match_term(&Term::Sum(RIGHT, String::from("Right"), vec![square.clone()])), None);
        assert_eq!(pat.match_term(&Term::Constructor(RIGHT, String::from("Right"))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(5))), None);
    }

    #[test]
    fn test_nested_irrefutable_pattern() {
        let irrefutable = Var(String::from("x"));
        // Just(Left(Right(x)))
        let pat = Sum(JUST, String::from("Just"), vec![Sum(LEFT, String::from("Left"), vec![Sum(RIGHT, String::from("Right"), vec![irrefutable])])]);
        let id = Term::Lambda(Box::new(Term::Var(0, String::from("y"))), String::from("id"));

        let matching_term = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(LEFT, String::from("Left"), vec![Term::Sum(RIGHT, String::from("Right"), vec![id.clone()])])]);
        assert_eq!(pat.match_term(&matching_term), Some(vec![id.clone()]));

        let not_quite = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(RIGHT, String::from("Right"), vec![Term::Sum(RIGHT, String::from("Right"), vec![id.clone()])])]);
        assert_eq!(pat.match_term(&not_quite), None);
    }

    #[test]
    fn test_nested_wildcard_pattern() {
        // Just(Left(Right(_)))
        let pat = Sum(JUST, String::from("Just"), vec![Sum(LEFT, String::from("Left"), vec![Sum(RIGHT, String::from("Right"), vec![Wildcard])])]);
        let id = Term::Lambda(Box::new(Term::Var(0, String::from("y"))), String::from("id"));

        let matching_term = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(LEFT, String::from("Left"), vec![Term::Sum(RIGHT, String::from("Right"), vec![id.clone()])])]);
        assert_eq!(pat.match_term(&matching_term), Some(vec![]));

        let not_quite = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(RIGHT, String::from("Right"), vec![Term::Sum(RIGHT, String::from("Right"), vec![id.clone()])])]);
        assert_eq!(pat.match_term(&not_quite), None);
    }


    #[test]
    fn test_nested_atom_pattern() {
        let atom = Atom(Atom::Int(42));
        // Just(Left(Right(42)))
        let pat = Sum(JUST, String::from("Just"), vec![Sum(LEFT, String::from("Left"), vec![Sum(RIGHT, String::from("Right"), vec![atom])])]);
        let answer = Term::Atom(Atom::Int(42));

        let matching_term = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(LEFT, String::from("Left"), vec![Term::Sum(RIGHT, String::from("Right"), vec![answer.clone()])])]);
        assert_eq!(pat.match_term(&matching_term), Some(vec![]));

        let wrong_structure = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(LEFT, String::from("Left"), vec![Term::Sum(RIGHT, String::from("Right"), vec![Term::Atom(Atom::Int(43))])])]);
        assert_eq!(pat.match_term(&wrong_structure), None);

        let wrong_structure = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(RIGHT, String::from("Right"), vec![Term::Sum(RIGHT, String::from("Right"), vec![answer.clone()])])]);
        assert_eq!(pat.match_term(&wrong_structure), None);
    }
}
