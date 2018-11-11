use super::Guid;
use codespan::{ByteIndex, ByteSpan};
use std::any::TypeId;
use std::fmt::{self, Display, Formatter};
use std::iter::{FromIterator, IntoIterator};

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
        Interface,
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

impl Display for Quote {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "cpp_quote({:?})", self.content)
    }
}

/// A type name.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Short,
    Long,
    Int,
    Char,
    Unsigned(Box<Type>),
    Void,
    Ptr { inner: Box<Type>, is_const: bool },
    Named(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Type::Short => write!(f, "short"),
            Type::Long => write!(f, "long"),
            Type::Int => write!(f, "int"),
            Type::Char => write!(f, "char"),
            Type::Unsigned(ref inner) => write!(f, "unsigned {}", inner),
            Type::Void => write!(f, "void"),
            Type::Ptr {
                ref inner,
                is_const,
            } => {
                if is_const {
                    write!(f, "const {} *", inner)
                } else {
                    write!(f, "{} *", inner)
                }
            }
            Type::Named(ref name) => write!(f, "{}", name),
        }
    }
}

/// A set of zero or more [`Annotation`]s.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Annotations {
    pub items: Vec<Annotation>,
    pub span: ByteSpan,
}

impl IntoIterator for Annotations {
    type Item = Annotation;
    type IntoIter = <Vec<Annotation> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a> IntoIterator for &'a Annotations {
    type Item = &'a Annotation;
    type IntoIter = <&'a [Annotation] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
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

        Annotations { items, span }
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

        Annotations { items, span }
    }
}

impl Display for Annotations {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[")?;

        if self.items.len() > 1 {
            self.items[0].fmt(f)?;
        }

        for item in self.items.iter().skip(1) {
            write!(f, " {}", item)?;
        }
        write!(f, "]")
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

impl Display for Annotation {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
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

impl Display for AnnotationKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            AnnotationKind::Local => write!(f, "local"),
            AnnotationKind::In => write!(f, "in"),
            AnnotationKind::Out => write!(f, "out"),
            AnnotationKind::Object => write!(f, "object"),
            AnnotationKind::Uuid(ref guid) => guid.fmt(f),
            AnnotationKind::Nested(ref name, ref inner) => write!(f, "{}({})", name, inner),
            AnnotationKind::Word(ref word) => word.fmt(f),
            AnnotationKind::StringLiteral(ref lit) => write!(f, "\"{}\"", lit),
        }
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

impl Display for FnDecl {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {}(", self.return_ty, self.name)?;

        if self.arguments.len() > 1 {
            write!(f, "{}", self.arguments[0])?;
        }

        for arg in self.arguments.iter().skip(1) {
            write!(f, ", {}", arg)?;
        }

        write!(f, ")")
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

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(ref annotations) = self.annotations {
            write!(f, "{} ", annotations)?;
        }

        write!(f, "{} {}", self.ty, self.name)
    }
}

/// A COM interface.
#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub name: String,
    pub base: Option<String>,
    pub annotations: Vec<Annotations>,
    pub items: Vec<FnDecl>,
    pub span: ByteSpan,
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

impl_ast_node!(Annotation);
impl_ast_node!(Annotations);
impl_ast_node!(Argument);
impl_ast_node!(Comment);
impl_ast_node!(FnDecl);
impl_ast_node!(Interface);
impl_ast_node!(Quote);
impl_ast_node!(Item; Comment | Quote | Interface);

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
            ("unsigned long", Type::Unsigned(Box::new(Type::Long))),
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
            (
                "[]",
                Annotations {
                    items: vec![],
                    span: span(0, 2),
                },
            ),
            (
                "[in]",
                Annotations {
                    span: span(0, 4),
                    items: vec![Annotation {
                        span: span(1, 3),
                        kind: AnnotationKind::In,
                    }],
                },
            ),
            (
                "[object]",
                Annotations {
                    span: span(0, 8),
                    items: vec![Annotation {
                        span: span(1, 7),
                        kind: AnnotationKind::Object,
                    }],
                },
            ),
            (
                "[uuid(00000000-0000-0000-C000-000000000046)]",
                Annotations {
                    span: span(0, 44),
                    items: vec![Annotation {
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
                    items: vec![Annotation {
                        span: span(1, 6),
                        kind: AnnotationKind::Local,
                    }],
                },
            ),
            (
                "[local, object]",
                Annotations {
                    span: span(0, 15),
                    items: vec![
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
                    items: vec![Annotation {
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
                        items: vec![Annotation {
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
                        items: vec![
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

        let got: Guid = src.parse().unwrap();

        assert_eq!(got, should_be);
    }

    #[test]
    fn parse_iunknown() {
        let src = IUNKNOWN;
        let should_be = Interface {
            name: "IUnknown".to_string(),
            base: None,
            annotations: vec![Annotations {
                items: vec![
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
                                items: vec![Annotation {
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
                                items: vec![
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
