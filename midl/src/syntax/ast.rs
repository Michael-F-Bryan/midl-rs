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
    #[derive(Debug, Clone, PartialEq)]
    pub enum Item {
        Quote,
        Comment,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comment {
    pub content: String,
    pub span: ByteSpan,
}

impl Comment {
    pub fn new(content: String, span: ByteSpan) -> Comment {
        Comment { content, span }
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

/// A type name.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Long,
    UnsignedLong,
    Other(String),
}

macro_rules! impl_ast_node {
    ($name:ty) => {
        impl AstNode for $name {
            fn span(&self) -> ByteSpan {
                self.span
            }
        }
    };
    ($name:ident; $( $variant:ident )|*) => {
        impl AstNode for $name {
            fn span(&self) -> ByteSpan {
                sum_type::defer!($name as *self; $($variant)|* => |ref item| <_ as AstNode>::span(item))
            }
        }

    }
}

impl_ast_node!(Quote);
impl_ast_node!(Comment);
impl_ast_node!(Item; Quote);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::grammar::{CommentParser, QuoteParser, TypeParser};

    #[test]
    fn parse_cpp_quote() {
        let src = r##"cpp_quote("#include <oaidl.h>")"##;
        let should_be = Quote::new("#include <oaidl.h>".to_string(), span(0, src.len()));

        let got = QuoteParser::new().parse(src).unwrap();

        assert_eq!(got, should_be);
    }

    #[test]
    fn builtin_types() {
        let inputs = vec![
            ("unsigned long", Type::UnsignedLong),
            ("long", Type::Long),
            ("HRESULT", Type::Other("HRESULT".to_string())),
        ];

        for (src, should_be) in inputs {
            let got = TypeParser::new()
                .parse(src)
                .expect(&format!("Can't parse {:?}", src));
            assert_eq!(got, should_be);
        }
    }

    #[test]
    fn comments() {
        let src = "// this is a comment";
        let should_be = Comment::new("this is a comment".to_string(), span(0, src.len()));

        let got = CommentParser::new().parse(src).unwrap();
        assert_eq!(got, should_be);
    }
}
