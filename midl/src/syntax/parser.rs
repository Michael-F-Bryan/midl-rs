use codespan::{CodeMap, FileMap, FileName};
use failure_derive::Fail;
use slog::{Discard, Logger};
use std::fmt::{self, Display, Formatter};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct Parser {
    imports: Arc<ImportResolver + Sync + Send>,
    logger: Logger,
}

impl Parser {
    pub fn new(logger: Logger) -> Parser {
        Parser::new_with_resolver(FileSystemImports::default(), logger)
    }

    pub fn new_with_resolver<I: ImportResolver + Sync + Send + 'static>(
        resolver: I,
        logger: Logger,
    ) -> Parser {
        Parser {
            imports: Arc::new(resolver),
            logger,
        }
    }

    pub fn imports(&self) -> &dyn ImportResolver {
        &*self.imports
    }
}

impl Default for Parser {
    fn default() -> Parser {
        Parser::new(Logger::root(Discard, slog::o!()))
    }
}

/// A mechanism for resolving file imports.
pub trait ImportResolver {
    fn add_filemap(&mut self, name: FileName, src: String) -> Arc<FileMap>;
    fn load(&self, path: &Path, source: Option<&FileName>) -> Result<Arc<FileMap>, ImportError>;
}

/// An importer which will search for imports based on a list of directories
/// it's been given.
#[derive(Debug, Default)]
pub struct FileSystemImports {
    map: Mutex<CodeMap>,
    paths: Vec<PathBuf>,
}

impl ImportResolver for FileSystemImports {
    fn load(&self, path: &Path, source: Option<&FileName>) -> Result<Arc<FileMap>, ImportError> {
        let map = self.map.lock().expect("The lock was poisoned");
        unimplemented!()
    }

    fn add_filemap(&mut self, name: FileName, src: String) -> Arc<FileMap> {
        self.map
            .lock()
            .expect("The lock was poisoned")
            .add_filemap(name, src)
    }
}

#[derive(Debug, Clone, PartialEq, Fail)]
pub enum ImportError {
    NotFound,
}

impl Display for ImportError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ImportError::NotFound => "Not Found".fmt(f),
        }
    }
}
