//! Parse raw MIDL files into an AST.

mod ast;
lalrpop_util::lalrpop_mod!(grammar, "/syntax/grammar.rs");

pub use self::ast::*;
