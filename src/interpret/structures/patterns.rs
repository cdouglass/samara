use interpret::structures::Term;

pub struct Pattern {
    pub n: usize, // constructor index
    pub tag: String, // constructor name 
    pub body: Box<Term>
}

impl Pattern {
    // TODO eventually return Option<HashMap<usize, Term>> - ie if it matches, give a substitution
    // of terms for variable indices
    fn match_pattern(&self, term: &Term) -> Option<HashMap<usize, Term>> {
        if let Term::Sum(ref n, _, _) = *term {
            *n == self.n
        } else {
            false
        }
    }
}
