# MIDL-rs

A Rust crate for parsing [MIDL] files and generating the corresponding COM
wrapper types.

> **Warning:** This project is still very much a work-in-progress and not yet
> fit for human consumption.

The project contains several crates:

- `midl`: a compiler frontend for parsing MIDL files, resolving imports, and 
  doing some basic validation
- `com_apis_generator`: an internal tool which can be used to generate Rust 
  interop code for the COM APIs declared in one or more MIDL files. It's the
  Rust equivalent of the `midl.exe` compiler supplied with Windows
- `com_apis`: a generated crate which exposes bindings for most of the COM 
  interfaces available as part of the Windows SDK
