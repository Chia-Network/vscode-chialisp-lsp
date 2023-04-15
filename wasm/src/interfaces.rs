use clvm_tools_rs::compiler::sexp::decode_string;

/// A simple abstract filesystem reader trait.
pub trait IFileReader {
    fn read_content(&self, name: &str) -> Result<String, String>;
}

/// A simple logger trait.
pub trait ILogWriter {
    fn log(&self, text: &str);
}

/// A real filesystem implementation of IFileReader.
#[derive(Default)]
pub struct FSFileReader {}

impl IFileReader for FSFileReader {
    fn read_content(&self, name: &str) -> Result<String, String> {
        std::fs::read(name)
            .map(|content| decode_string(&content))
            .map_err(|e| format!("{e:?}"))
    }
}

impl FSFileReader {
    #[cfg(test)]
    pub fn new() -> Self {
        Default::default()
    }
}

/// A real stderr logger implementation of ILogWriter.
#[derive(Default)]
pub struct EPrintWriter {}

impl ILogWriter for EPrintWriter {
    fn log(&self, text: &str) {
        eprintln!("{text}");
    }
}

impl EPrintWriter {
    #[cfg(test)]
    pub fn new() -> Self {
        Default::default()
    }
}
