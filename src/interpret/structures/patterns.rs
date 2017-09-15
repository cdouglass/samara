use interpret::structures::Atom;
use interpret::structures::Term;

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub enum Pattern {
    Wildcard,
    Sum(usize, String, Box<Pattern>),
    Atom(Atom),
    Var(usize, String)
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub enum Match {
    Binding(usize, Term),
    Plain
}

impl Pattern {
    pub fn match_term(&self, term: &Term) -> Option<Match> {
        match (self, term) {
            (&Pattern::Wildcard, _) => Some(Match::Plain),
            (&Pattern::Sum(ref n, _, ref pat), &Term::Sum(ref m, _, ref values)) => {
                if m == n {
                    // assuming for now there will either be 1 or 0
                    // so it doesn't matter which end we look at
                    match values.last() {
                        Some(val) => pat.match_term(val),
                        None => pat.match_term(&Term::Atom(Atom::Unit))
                    }
                } else { None }
            },
            (&Pattern::Atom(ref a), &Term::Atom(ref b)) => {
                if a == b {
                    Some(Match::Plain)
                } else { None }
            },
            (&Pattern::Var(ref n, _), _) => {
                Some(Match::Binding(*n, term.clone()))
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

    #[test]
    fn test_wildcard_pattern() {
        let pat = Wildcard;
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(5))), Some(Match::Plain));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), Some(Match::Plain));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(true))), Some(Match::Plain));
    }

    #[test]
    fn test_atom_pattern() {
        let pat = Atom(Atom::Int(5));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(5))), Some(Match::Plain));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(true))), None);

        let pat = Atom(Atom::Bool(true));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(true))), Some(Match::Plain));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(false))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);

        let pat = Atom(Atom::Unit);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Unit)), Some(Match::Plain));
        assert_eq!(pat.match_term(&Term::Atom(Atom::Bool(false))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);

        let pat = Atom(Atom::BuiltIn(Op::Mul));
        assert_eq!(pat.match_term(&Term::Atom(Atom::BuiltIn(Op::Mul))), Some(Match::Plain));
        assert_eq!(pat.match_term(&Term::Atom(Atom::BuiltIn(Op::Add))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(42))), None);
    }

    #[test]
    fn test_irrefutable_pattern() {
        let irrefutable = Var(1, String::from("x"));
        let five = Term::Atom(Atom::Int(5));
        let square = Term::Lambda(Box::new(Term::App(Box::new(Term::App(Box::new(Term::Atom(Atom::BuiltIn(Op::Mul))), Box::new(Term::Var(0, String::from("y"))))), Box::new(Term::Var(0, String::from("y"))))), String::from("square"));
        let none = Term::Sum(NONE, String::from("None"), vec![]);
        let unit = Term::Atom(Atom::Unit);

        assert_eq!(irrefutable.match_term(&five), Some(Match::Binding(1, five)));
        assert_eq!(irrefutable.match_term(&square), Some(Match::Binding(1, square)));
        assert_eq!(irrefutable.match_term(&none), Some(Match::Binding(1, none)));
        assert_eq!(irrefutable.match_term(&unit), Some(Match::Binding(1, unit)));
    }

    #[test]
    fn test_sum_pattern() {
        let irrefutable = Var(1, String::from("x"));
        let pat = Sum(LEFT, String::from("Left"), Box::new(irrefutable));
        let square = Term::Lambda(Box::new(Term::App(Box::new(Term::App(Box::new(Term::Atom(Atom::BuiltIn(Op::Mul))), Box::new(Term::Var(0, String::from("y"))))), Box::new(Term::Var(0, String::from("y"))))), String::from("square"));

        let expected = Match::Binding(1, square.clone());
        let actual = pat.match_term(&Term::Sum(LEFT, String::from("Left"), vec![square.clone()]));
        assert_eq!(actual, Some(expected));

        assert_eq!(pat.match_term(&Term::Sum(RIGHT, String::from("Right"), vec![square.clone()])), None);
        assert_eq!(pat.match_term(&Term::Constructor(RIGHT, String::from("Right"))), None);
        assert_eq!(pat.match_term(&Term::Atom(Atom::Int(5))), None);
    }

    #[test]
    fn test_nested_irrefutable_pattern() {
        let irrefutable = Var(1, String::from("x"));
        // Just(Left(Right(x)))
        let pat = Sum(JUST, String::from("Just"), Box::new(Sum(LEFT, String::from("Left"), Box::new(Sum(RIGHT, String::from("Right"), Box::new(irrefutable))))));
        let id = Term::Lambda(Box::new(Term::Var(0, String::from("y"))), String::from("id"));

        let matching_term = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(LEFT, String::from("Left"), vec![Term::Sum(RIGHT, String::from("Right"), vec![id.clone()])])]);
        assert_eq!(pat.match_term(&matching_term), Some(Match::Binding(1, id.clone())));

        let not_quite = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(RIGHT, String::from("Right"), vec![Term::Sum(RIGHT, String::from("Right"), vec![id.clone()])])]);
        assert_eq!(pat.match_term(&not_quite), None);
    }

    #[test]
    fn test_nested_wildcard_pattern() {
        // Just(Left(Right(_)))
        let pat = Sum(JUST, String::from("Just"), Box::new(Sum(LEFT, String::from("Left"), Box::new(Sum(RIGHT, String::from("Right"), Box::new(Wildcard))))));
        let id = Term::Lambda(Box::new(Term::Var(0, String::from("y"))), String::from("id"));

        let matching_term = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(LEFT, String::from("Left"), vec![Term::Sum(RIGHT, String::from("Right"), vec![id.clone()])])]);
        assert_eq!(pat.match_term(&matching_term), Some(Match::Plain));

        let not_quite = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(RIGHT, String::from("Right"), vec![Term::Sum(RIGHT, String::from("Right"), vec![id.clone()])])]);
        assert_eq!(pat.match_term(&not_quite), None);
    }


    #[test]
    fn test_nested_atom_pattern() {
        let atom = Atom(Atom::Int(42));
        // Just(Left(Right(42)))
        let pat = Sum(JUST, String::from("Just"), Box::new(Sum(LEFT, String::from("Left"), Box::new(Sum(RIGHT, String::from("Right"), Box::new(atom))))));
        let answer = Term::Atom(Atom::Int(42));

        let matching_term = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(LEFT, String::from("Left"), vec![Term::Sum(RIGHT, String::from("Right"), vec![answer.clone()])])]);
        assert_eq!(pat.match_term(&matching_term), Some(Match::Plain));

        let wrong_structure = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(LEFT, String::from("Left"), vec![Term::Sum(RIGHT, String::from("Right"), vec![Term::Atom(Atom::Int(43))])])]);
        assert_eq!(pat.match_term(&wrong_structure), None);

        let wrong_structure = Term::Sum(JUST, String::from("Just"), vec![Term::Sum(RIGHT, String::from("Right"), vec![Term::Sum(RIGHT, String::from("Right"), vec![answer.clone()])])]);
        assert_eq!(pat.match_term(&wrong_structure), None);
    }
}
