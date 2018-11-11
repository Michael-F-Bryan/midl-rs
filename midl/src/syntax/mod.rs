//! Parse raw MIDL files into an AST.

pub mod ast;
mod guid;
pub mod parser;
pub mod visit_mut;
lalrpop_util::lalrpop_mod!(grammar, "/syntax/grammar.rs");

pub use self::guid::Guid;
pub use self::parser::Parser;
pub use self::visit_mut::MutVisitor;
