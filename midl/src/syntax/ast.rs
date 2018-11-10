use codespan::{ByteIndex, ByteSpan};
use std::any::TypeId;

pub(crate) fn span(l: usize, r: usize) -> ByteSpan {
    ByteSpan::new(ByteIndex(l as u32), ByteIndex(r as u32))
}

pub trait AstNode: 'static {
    fn span(&self) -> ByteSpan;

    #[doc(hidden)]
    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
}

sum_type::sum_type! {
    pub enum Item {
        Quote(Quote),
        Int(i32),
    }
}

/// Code which should be copied into the output file as-is. Corresponds to the
/// `cpp_quote()` statement.
#[derive(Debug, Clone, PartialEq)]
pub struct Quote {
    pub content: String,
    pub span: ByteSpan,
}

impl Quote {
    pub fn new(content: String, span: ByteSpan) -> Quote {
        Quote { content, span }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::grammar::QuoteParser;

    #[test]
    fn parse_cpp_quote() {
        let src = r##"cpp_quote("#include <oaidl.h>")"##;
        let should_be = Quote::new("#include <oaidl.h>".to_string(), span(0, src.len()));

        let got = QuoteParser::new().parse(src).unwrap();

        assert_eq!(got, should_be);
    }
}
