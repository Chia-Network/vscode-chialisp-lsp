pub trait MessageHandler<M> {
    fn handle_message(
        &mut self,
        raw_json: &serde_json::Value,
        msg: &M,
    ) -> Result<Option<Vec<M>>, String>;
}
