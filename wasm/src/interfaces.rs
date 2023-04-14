use clvm_tools_rs::compiler::sexp::decode_string;

pub trait IFileReader {
    fn read_content(&self, name: &str) -> Result<String, String>;
}

pub trait ILogWriter {
    fn log(&self, text: &str);
}

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
