use codespan::{CodeMap, FileMap, FileName};
use failure_derive::Fail;
use slog::{Discard, Logger};
use std::env;
use std::fmt::{self, Display, Formatter};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct Parser {
    imports: Arc<ImportResolver>,
    logger: Logger,
}

impl Parser {
    pub fn new(logger: Logger) -> Parser {
        Parser::new_with_resolver(FileSystemImports::default(), logger)
    }

    pub fn new_with_resolver<I: ImportResolver>(resolver: I, logger: Logger) -> Parser {
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
pub trait ImportResolver: Send + Sync + 'static {
    fn add_filemap(&self, name: FileName, src: String) -> Arc<FileMap>;
    fn load(&self, path: &Path, source: Option<&FileName>) -> Result<Arc<FileMap>, ImportError>;
}

/// An importer which will search for imports based on a list of directories
/// it's been given.
#[derive(Debug)]
pub struct FileSystemImports {
    logger: Logger,
    map: Mutex<CodeMap>,
    paths: Vec<PathBuf>,
}

impl FileSystemImports {
    fn normalize(&self, path: &Path, source: Option<&FileName>) -> PathBuf {
        if path.is_absolute() {
            path.to_path_buf()
        } else if path.components().count() > 2 {
            match source {
                Some(parent) => unimplemented!(),
                None => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
    }
}

impl Default for FileSystemImports {
    fn default() -> FileSystemImports {
        let mut paths = Vec::new();

        if let Ok(current_dir) = env::current_dir() {
            paths.push(current_dir);
        }

        FileSystemImports {
            logger: Logger::root(Discard, slog::o!()),
            map: Default::default(),
            paths,
        }
    }
}

impl ImportResolver for FileSystemImports {
    fn load(&self, path: &Path, source: Option<&FileName>) -> Result<Arc<FileMap>, ImportError> {
        let path = self.normalize(path, source);
        let filename = FileName::from(path.clone());

        let mut map = self.map.lock().expect("The lock was poisoned");

        if let Some(cached) = map.iter().find(|fm| fm.name() == &filename) {
            return Ok(Arc::clone(cached));
        }

        slog::debug!(self.logger, "Trying to load an import from disk";
            "filename" => path.display(),
            "source" => source.map(ToString::to_string));

        if !path.exists() {
            Err(ImportError::NotFound)
        } else {
            map.add_filemap_from_disk(path).map_err(ImportError::Io)
        }
    }

    fn add_filemap(&self, name: FileName, src: String) -> Arc<FileMap> {
        self.map
            .lock()
            .expect("The lock was poisoned")
            .add_filemap(name, src)
    }
}

/// An error that may occur while importing a file.
#[derive(Debug, Fail)]
pub enum ImportError {
    NotFound,
    Io(io::Error),
}

impl Display for ImportError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ImportError::NotFound => "Not Found".fmt(f),
            ImportError::Io(ref inner) => inner.fmt(f),
        }
    }
}
