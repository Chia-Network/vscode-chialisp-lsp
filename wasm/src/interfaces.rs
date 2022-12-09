pub trait IFileReader {
    fn read(&self, name: &str) -> Result<Vec<u8>, String>;
}

pub trait ILogWriter {
    fn write(&self, text: &str);
}

#[derive(Default)]
pub struct FSFileReader {}

impl IFileReader for FSFileReader {
    fn read(&self, name: &str) -> Result<Vec<u8>, String> {
        std::fs::read(name).map_err(|e| format!("{:?}", e))
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
    fn write(&self, text: &str) {
        eprintln!("{}", text);
    }
}

impl EPrintWriter {
    #[cfg(test)]
    pub fn new() -> Self {
        Default::default()
    }
}
