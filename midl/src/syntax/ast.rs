use codespan::{ByteIndex, ByteSpan};
use failure_derive::Fail;
use regex::Regex;
use std::any::TypeId;
use std::fmt::{self, Display, Formatter};
use std::iter::{FromIterator, IntoIterator};
use std::str::FromStr;

pub(crate) fn span(l: usize, r: usize) -> ByteSpan {
    ByteSpan::new(ByteIndex(l as u32), ByteIndex(r as u32))
}

/// A trait implemented by all AST nodes.
pub trait AstNode: 'static {
    fn span(&self) -> ByteSpan;

    #[doc(hidden)]
    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
}

sum_type::sum_type! {
    /// A top-level item.
    #[derive(Debug, Clone, PartialEq)]
    pub enum Item {
        Quote,
        Comment,
    }
}

/// A comment.
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
    Void,
    ConstPtr(Box<Type>),
    Ptr { inner: Box<Type>, is_const: bool },
    Named(String),
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

/// A set of zero or more [`Annotation`]s.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Annotations {
    pub inner: Vec<Annotation>,
    pub span: ByteSpan,
}

impl Annotations {
    pub fn new(inner: Vec<Annotation>, span: ByteSpan) -> Annotations {
        Annotations { inner, span }
    }
}

impl IntoIterator for Annotations {
    type Item = Annotation;
    type IntoIter = <Vec<Annotation> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a> IntoIterator for &'a Annotations {
    type Item = &'a Annotation;
    type IntoIter = <&'a [Annotation] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl FromIterator<Annotation> for Annotations {
    fn from_iter<I: IntoIterator<Item = Annotation>>(iter: I) -> Annotations {
        let mut items = Vec::new();
        let mut span = ByteSpan::default();

        for item in iter {
            span = if span == ByteSpan::default() {
                item.span()
            } else {
                span.to(item.span())
            };
            items.push(item);
        }

        Annotations::new(items, span)
    }
}

impl FromIterator<Annotations> for Annotations {
    fn from_iter<I: IntoIterator<Item = Annotations>>(iter: I) -> Annotations {
        let mut span = ByteSpan::default();
        let items = iter
            .into_iter()
            .inspect(|ann| {
                if span == Default::default() {
                    span = ann.span();
                } else {
                    span = span.to(ann.span())
                }
            })
            .flatten()
            .collect();

        Annotations::new(items, span)
    }
}

/// A single annotation.
#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub kind: AnnotationKind,
    pub span: ByteSpan,
}

impl Annotation {
    pub fn new(kind: AnnotationKind, span: ByteSpan) -> Annotation {
        Annotation { kind, span }
    }
}

/// The actual type of annotation this is.
#[derive(Debug, Clone, PartialEq)]
pub enum AnnotationKind {
    Local,
    In,
    Out,
    Object,
    Uuid(Guid),
    Nested(String, Box<Annotation>),
    Word(String),
    StringLiteral(String),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Guid {
    pub data1: u32,
    pub data2: u16,
    pub data3: u16,
    pub data4: [u8; 8],
}

impl FromStr for Guid {
    type Err = ParseGuidError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static::lazy_static! {
            static ref PATTERN: Regex = Regex::new(r"^([[:xdigit:]]{8})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{12})$").unwrap();
        }

        let got = PATTERN.captures(s).ok_or(ParseGuidError)?;

        let first_u16 = u16::from_str_radix(&got[4], 16).unwrap();
        let data4 = &got[5];
        let data4 = [
            (first_u16 >> 8) as u8,
            (first_u16 & 0xFF) as u8,
            u8::from_str_radix(&data4[0..2], 16).unwrap(),
            u8::from_str_radix(&data4[2..4], 16).unwrap(),
            u8::from_str_radix(&data4[4..6], 16).unwrap(),
            u8::from_str_radix(&data4[6..8], 16).unwrap(),
            u8::from_str_radix(&data4[8..10], 16).unwrap(),
            u8::from_str_radix(&data4[10..12], 16).unwrap(),
        ];

        Ok(Guid {
            data1: u32::from_str_radix(&got[1], 16).unwrap(),
            data2: u16::from_str_radix(&got[2], 16).unwrap(),
            data3: u16::from_str_radix(&got[3], 16).unwrap(),
            data4,
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Fail)]
#[fail(display = "Invalid GUID format")]
pub struct ParseGuidError;

impl Display for Guid {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{:08X}-{:04X}-{:04X}-{:02X}{:02X}-",
            self.data1, self.data2, self.data3, self.data4[0], self.data4[1],
        )?;
        for byte in &self.data4[2..] {
            write!(f, "{:02X}", byte)?;
        }

        Ok(())
    }
}

/// A bare function signature.
#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub name: String,
    pub return_ty: Type,
    pub arguments: Vec<Argument>,
    pub span: ByteSpan,
}

impl FnDecl {
    pub fn new(name: String, return_ty: Type, arguments: Vec<Argument>, span: ByteSpan) -> FnDecl {
        FnDecl {
            name,
            return_ty,
            arguments,
            span,
        }
    }
}

/// A function argument.
#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub name: String,
    pub ty: Type,
    pub annotations: Option<Annotations>,
    pub span: ByteSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub name: String,
    pub base: Option<String>,
    pub annotations: Vec<Annotations>,
    pub items: Vec<FnDecl>,
    pub span: ByteSpan,
}

impl_ast_node!(Annotation);
impl_ast_node!(Annotations);
impl_ast_node!(Argument);
impl_ast_node!(FnDecl);
impl_ast_node!(Comment);
impl_ast_node!(Quote);
impl_ast_node!(Item; Comment | Quote);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::grammar::*;

    // NOTE: We've hacked this so `const GUID * riid` is now `GUID * riid`.
    const IUNKNOWN: &str = r#"
[
    local,
    object,
    uuid(00000000-0000-0000-C000-000000000046),

    pointer_default(unique)
]
interface IUnknown
{
    HRESULT QueryInterface([in] GUID * riid, [out, iid_is(riid), annotation("__RPC__deref_out")] void **ppvObject);
    ULONG AddRef();
    ULONG Release();
};
    "#;

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
            (
                "void *",
                Type::Ptr {
                    inner: Box::new(Type::Void),
                    is_const: false,
                },
            ),
            ("HRESULT", Type::Named("HRESULT".to_string())),
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

    #[test]
    fn annotations() {
        let inputs = vec![
            ("[]", Annotations::new(vec![], span(0, 2))),
            (
                "[in]",
                Annotations {
                    span: span(0, 4),
                    inner: vec![Annotation {
                        span: span(1, 3),
                        kind: AnnotationKind::In,
                    }],
                },
            ),
            (
                "[object]",
                Annotations {
                    span: span(0, 8),
                    inner: vec![Annotation {
                        span: span(1, 7),
                        kind: AnnotationKind::Object,
                    }],
                },
            ),
            (
                "[uuid(00000000-0000-0000-C000-000000000046)]",
                Annotations {
                    span: span(0, 44),
                    inner: vec![Annotation {
                        span: span(1, 43),
                        kind: AnnotationKind::Uuid(
                            "00000000-0000-0000-C000-000000000046".parse().unwrap(),
                        ),
                    }],
                },
            ),
            (
                "[local]",
                Annotations {
                    span: span(0, 7),
                    inner: vec![Annotation {
                        span: span(1, 6),
                        kind: AnnotationKind::Local,
                    }],
                },
            ),
            (
                "[local, object]",
                Annotations {
                    span: span(0, 15),
                    inner: vec![
                        Annotation {
                            span: span(1, 6),
                            kind: AnnotationKind::Local,
                        },
                        Annotation {
                            span: span(8, 14),
                            kind: AnnotationKind::Object,
                        },
                    ],
                },
            ),
            (
                "[something_else]",
                Annotations {
                    span: span(0, 16),
                    inner: vec![Annotation {
                        span: span(1, 15),
                        kind: AnnotationKind::Word("something_else".to_string()),
                    }],
                },
            ),
        ];

        for (src, should_be) in inputs {
            let got = AnnotationsParser::new().parse(src).unwrap();
            assert_eq!(got, should_be);
        }
    }

    #[test]
    fn basic_function_signature() {
        let src = "ULONG Release()";
        let should_be = FnDecl::new(
            "Release".to_string(),
            Type::Named("ULONG".to_string()),
            vec![],
            span(0, src.len()),
        );

        let got = FnDeclParser::new().parse(src).unwrap();

        assert_eq!(got, should_be);
    }

    #[test]
    fn function_with_arguments() {
        let src = r#"ULONG Release([in] GUID * riid, [out, iid_is(riid)] void **ppvObject)"#;
        let should_be = FnDecl {
            name: "Release".to_string(),
            return_ty: Type::Named("ULONG".to_string()),
            arguments: vec![
                Argument {
                    name: "riid".to_string(),
                    ty: Type::Ptr {
                        inner: Box::new(Type::Named("GUID".to_string())),
                        is_const: false,
                    },
                    annotations: Some(Annotations {
                        inner: vec![Annotation {
                            kind: AnnotationKind::In,
                            span: span(15, 17),
                        }],
                        span: span(14, 18),
                    }),
                    span: span(14, 30),
                },
                Argument {
                    name: "ppvObject".to_string(),
                    ty: Type::Ptr {
                        inner: Box::new(Type::Ptr {
                            inner: Box::new(Type::Void),
                            is_const: false,
                        }),
                        is_const: false,
                    },
                    annotations: Some(Annotations {
                        inner: vec![
                            Annotation {
                                kind: AnnotationKind::Out,
                                span: span(33, 36),
                            },
                            Annotation {
                                kind: AnnotationKind::Nested(
                                    "iid_is".to_string(),
                                    Box::new(Annotation {
                                        kind: AnnotationKind::Word("riid".to_string()),
                                        span: span(45, 49),
                                    }),
                                ),
                                span: span(38, 50),
                            },
                        ],
                        span: span(32, 51),
                    }),
                    span: span(32, 68),
                },
            ],
            span: span(0, src.len()),
        };

        let got = FnDeclParser::new().parse(src).unwrap();

        assert_eq!(got, should_be);
    }

    #[test]
    fn print_guid() {
        let guid = Guid {
            data1: 0,
            data2: 0,
            data3: 0x0000,
            data4: [0xC0, 0, 0, 0, 0, 0, 0, 0x46],
        };
        let should_be = "00000000-0000-0000-C000-000000000046";

        let got = guid.to_string();

        assert_eq!(got, should_be);
    }

    #[test]
    fn parse_a_guid() {
        let should_be = Guid {
            data1: 0,
            data2: 0,
            data3: 0x0000,
            data4: [0xC0, 0, 0, 0, 0, 0, 0, 0x46],
        };
        let src = "00000000-0000-0000-C000-000000000046";

        let got = Guid::from_str(src).unwrap();

        assert_eq!(got, should_be);
    }

    #[test]
    fn parse_iunknown() {
        let src = IUNKNOWN;
        let should_be = Interface {
            name: "IUnknown".to_string(),
            base: None,
            annotations: vec![Annotations {
                inner: vec![
                    Annotation {
                        kind: AnnotationKind::Local,
                        span: span(7, 12),
                    },
                    Annotation {
                        kind: AnnotationKind::Object,
                        span: span(18, 24),
                    },
                    Annotation {
                        kind: AnnotationKind::Uuid(Guid {
                            data1: 0,
                            data2: 0,
                            data3: 0,
                            data4: [192, 0, 0, 0, 0, 0, 0, 70],
                        }),
                        span: span(30, 72),
                    },
                    Annotation {
                        kind: AnnotationKind::Nested(
                            "pointer_default".to_string(),
                            Box::new(Annotation {
                                kind: AnnotationKind::Word("unique".to_string()),
                                span: span(95, 101),
                            }),
                        ),
                        span: span(79, 102),
                    },
                ],
                span: span(1, 104),
            }],
            items: vec![
                FnDecl {
                    name: "QueryInterface".to_string(),
                    return_ty: Type::Named("HRESULT".to_string()),
                    arguments: vec![
                        Argument {
                            name: "riid".to_string(),
                            ty: Type::Ptr {
                                inner: Box::new(Type::Named("GUID".to_string())),
                                is_const: false,
                            },
                            annotations: Some(Annotations {
                                inner: vec![Annotation {
                                    kind: AnnotationKind::In,
                                    span: span(154, 156),
                                }],
                                span: span(153, 157),
                            }),
                            span: span(153, 169),
                        },
                        Argument {
                            name: "ppvObject".to_string(),
                            ty: Type::Ptr {
                                inner: Box::new(Type::Ptr {
                                    inner: Box::new(Type::Void),
                                    is_const: false,
                                }),
                                is_const: false,
                            },
                            annotations: Some(Annotations {
                                inner: vec![
                                    Annotation {
                                        kind: AnnotationKind::Out,
                                        span: span(172, 175),
                                    },
                                    Annotation {
                                        kind: AnnotationKind::Nested(
                                            "iid_is".to_string(),
                                            Box::new(Annotation {
                                                kind: AnnotationKind::Word("riid".to_string()),
                                                span: span(184, 188),
                                            }),
                                        ),
                                        span: span(177, 189),
                                    },
                                    Annotation {
                                        kind: AnnotationKind::Nested(
                                            "annotation".to_string(),
                                            Box::new(Annotation {
                                                kind: AnnotationKind::StringLiteral(
                                                    "__RPC__deref_out".to_string(),
                                                ),
                                                span: span(202, 220),
                                            }),
                                        ),
                                        span: span(191, 221),
                                    },
                                ],
                                span: span(171, 222),
                            }),
                            span: span(171, 239),
                        },
                    ],
                    span: span(130, 240),
                },
                FnDecl {
                    name: "AddRef".to_string(),
                    return_ty: Type::Named("ULONG".to_string()),
                    arguments: vec![],
                    span: span(246, 260),
                },
                FnDecl {
                    name: "Release".to_string(),
                    return_ty: Type::Named("ULONG".to_string()),
                    arguments: vec![],
                    span: span(266, 281),
                },
            ],
            span: span(1, 285),
        };

        let got = InterfaceParser::new().parse(src).unwrap();

        assert_eq!(got, should_be);
    }
}
