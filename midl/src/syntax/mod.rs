//! Parse raw MIDL files into an AST.

mod ast;
mod guid;
lalrpop_util::lalrpop_mod!(grammar, "/syntax/grammar.rs");

pub use self::ast::*;
pub use self::guid::Guid;
