use super::ast::{
    Annotation, Annotations, Argument, Comment, File, FnDecl, Imports, Interface, Item, Quote,
};
use codespan::ByteSpan;

/// Mutably visit an AST node.
pub trait MutVisitor {
    fn visit_file_mut(&mut self, file: &mut File) {
        visit_file_mut(self, file);
    }

    fn visit_item_mut(&mut self, item: &mut Item) {
        visit_item_mut(self, item);
    }

    fn visit_comment_mut(&mut self, comment: &mut Comment) {
        visit_comment_mut(self, comment);
    }

    fn visit_interface_mut(&mut self, interface: &mut Interface) {
        visit_interface_mut(self, interface);
    }

    fn visit_imports_mut(&mut self, imports: &mut Imports) {
        visit_imports_mut(self, imports);
    }

    fn visit_quote_mut(&mut self, quote: &mut Quote) {
        visit_quote_mut(self, quote);
    }

    fn visit_fn_decl_mut(&mut self, decl: &mut FnDecl) {
        visit_fn_decl_mut(self, decl);
    }

    fn visit_function_argument_mut(&mut self, arg: &mut Argument) {
        visit_function_argument_mut(self, arg);
    }

    fn visit_annotations_mut(&mut self, annotations: &mut Annotations) {
        visit_annotations_mut(self, annotations);
    }

    fn visit_annotation_mut(&mut self, annotations: &mut Annotation) {
        visit_annotation_mut(self, annotations);
    }
}

fn visit_file_mut<V: MutVisitor + ?Sized>(visitor: &mut V, file: &mut File) {
    for item in &mut file.items {
        visitor.visit_item_mut(item);
    }
}

fn visit_item_mut<V: MutVisitor + ?Sized>(visitor: &mut V, item: &mut Item) {
    match *item {
        Item::Quote(ref mut quote) => visitor.visit_quote_mut(quote),
        Item::Interface(ref mut interface) => visitor.visit_interface_mut(interface),
        Item::Comment(ref mut comment) => visitor.visit_comment_mut(comment),
        Item::Imports(ref mut imports) => visitor.visit_imports_mut(imports),
    }
}

fn visit_quote_mut<V: MutVisitor + ?Sized>(visitor: &mut V, quote: &mut Quote) {}

fn visit_interface_mut<V: MutVisitor + ?Sized>(visitor: &mut V, interface: &mut Interface) {
    for annotations in &mut interface.annotations {
        visitor.visit_annotations_mut(annotations);
    }

    for item in &mut interface.items {
        visitor.visit_fn_decl_mut(item);
    }
}

fn visit_comment_mut<V: MutVisitor + ?Sized>(visitor: &mut V, comment: &mut Comment) {}

fn visit_imports_mut<V: MutVisitor + ?Sized>(visitor: &mut V, imports: &mut Imports) {}

fn visit_annotations_mut<V: MutVisitor + ?Sized>(visitor: &mut V, annotations: &mut Annotations) {
    for item in &mut annotations.items {
        visitor.visit_annotation_mut(item);
    }
}

fn visit_annotation_mut<V: MutVisitor + ?Sized>(visitor: &mut V, annotation: &mut Annotation) {}

fn visit_fn_decl_mut<V: MutVisitor + ?Sized>(visitor: &mut V, decl: &mut FnDecl) {
    for arg in &mut decl.arguments {
        visitor.visit_function_argument_mut(arg);
    }
}

fn visit_function_argument_mut<V: MutVisitor + ?Sized>(visitor: &mut V, arg: &mut Argument) {}

/// Apply a transformation to every [`ByteSpan`] in the AST.
pub struct MapSpans<F> {
    map: F,
}

impl<F: FnMut(ByteSpan) -> ByteSpan> MapSpans<F> {
    pub fn new(map: F) -> MapSpans<F> {
        MapSpans { map }
    }
}

impl<F: FnMut(ByteSpan) -> ByteSpan> MutVisitor for MapSpans<F> {
    fn visit_file_mut(&mut self, file: &mut File) {
        file.span = (self.map)(file.span);
        visit_file_mut(self, file);
    }

    fn visit_item_mut(&mut self, item: &mut Item) {
        visit_item_mut(self, item);
    }

    fn visit_comment_mut(&mut self, comment: &mut Comment) {
        comment.span = (self.map)(comment.span);
        visit_comment_mut(self, comment);
    }

    fn visit_interface_mut(&mut self, interface: &mut Interface) {
        interface.span = (self.map)(interface.span);
        visit_interface_mut(self, interface);
    }

    fn visit_imports_mut(&mut self, imports: &mut Imports) {
        imports.span = (self.map)(imports.span);
        visit_imports_mut(self, imports);
    }

    fn visit_quote_mut(&mut self, quote: &mut Quote) {
        quote.span = (self.map)(quote.span);
        visit_quote_mut(self, quote);
    }

    fn visit_fn_decl_mut(&mut self, decl: &mut FnDecl) {
        decl.span = (self.map)(decl.span);
        visit_fn_decl_mut(self, decl);
    }

    fn visit_function_argument_mut(&mut self, arg: &mut Argument) {
        arg.span = (self.map)(arg.span);
        visit_function_argument_mut(self, arg);
    }

    fn visit_annotations_mut(&mut self, annotations: &mut Annotations) {
        annotations.span = (self.map)(annotations.span);
        visit_annotations_mut(self, annotations);
    }

    fn visit_annotation_mut(&mut self, annotation: &mut Annotation) {
        annotation.span = (self.map)(annotation.span);
        visit_annotation_mut(self, annotation);
    }
}
