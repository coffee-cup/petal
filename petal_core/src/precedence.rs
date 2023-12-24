#[derive(ToPrimitive, FromPrimitive, PartialOrd, PartialEq, Clone, Debug)]
pub enum Precedence {
    Lowest,
    Assign,      // =
    Conditional, // ?:
    LogicalOr,   // ||
    LogicalAnd,  // &&
    Equality,    // ==, !=
    Comparison,  // <, >, <=, >=
    Sum,         // +, -
    Product,     // *, /
    Exponent,    // ^
    Unary,       // !, -
    Call,        // my_function()
    Highest,
}

impl Precedence {
    pub fn next(&self) -> Precedence {
        let value = num::ToPrimitive::to_usize(self).unwrap();
        num::FromPrimitive::from_usize(value + 1).unwrap_or(Precedence::Highest)
    }

    pub fn prev(&self) -> Precedence {
        let value = num::ToPrimitive::to_usize(self).unwrap();
        match value {
            0 => Precedence::Lowest,
            v => num::FromPrimitive::from_usize(v - 1).unwrap(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_precedence() {
        assert_eq!(Precedence::Lowest.next(), Precedence::Assign);
        assert_eq!(Precedence::Assign.next(), Precedence::Conditional);
        assert_eq!(Precedence::Conditional.next(), Precedence::LogicalOr);
        assert_eq!(Precedence::LogicalOr.next(), Precedence::LogicalAnd);
        assert_eq!(Precedence::LogicalAnd.next(), Precedence::Equality);
        assert_eq!(Precedence::Equality.next(), Precedence::Comparison);
        assert_eq!(Precedence::Comparison.next(), Precedence::Sum);
        assert_eq!(Precedence::Sum.next(), Precedence::Product);
        assert_eq!(Precedence::Product.next(), Precedence::Exponent);
        assert_eq!(Precedence::Exponent.next(), Precedence::Unary);
        assert_eq!(Precedence::Unary.next(), Precedence::Call);
        assert_eq!(Precedence::Call.next(), Precedence::Highest);
        assert_eq!(Precedence::Highest.next(), Precedence::Highest);
    }

    #[test]
    fn prev_precendence() {
        assert_eq!(Precedence::Lowest.prev(), Precedence::Lowest);
        assert_eq!(Precedence::Assign.prev(), Precedence::Lowest);
        assert_eq!(Precedence::Conditional.prev(), Precedence::Assign);
        assert_eq!(Precedence::LogicalOr.prev(), Precedence::Conditional);
        assert_eq!(Precedence::LogicalAnd.prev(), Precedence::LogicalOr);
        assert_eq!(Precedence::Equality.prev(), Precedence::LogicalAnd);
        assert_eq!(Precedence::Comparison.prev(), Precedence::Equality);
        assert_eq!(Precedence::Sum.prev(), Precedence::Comparison);
        assert_eq!(Precedence::Product.prev(), Precedence::Sum);
        assert_eq!(Precedence::Exponent.prev(), Precedence::Product);
        assert_eq!(Precedence::Unary.prev(), Precedence::Exponent);
        assert_eq!(Precedence::Call.prev(), Precedence::Unary);
        assert_eq!(Precedence::Highest.prev(), Precedence::Call);
    }
}
