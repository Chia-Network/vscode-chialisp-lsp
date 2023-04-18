// Based on https://docs.rs/lsp-server/latest/src/lsp_server/lib.rs.html#27-30
// and https://github.com/Chia-Network/vscode-chialisp-lsp/blob/main/runner/src/runer.js
use crate::dbg::types::MessageHandler;
use clvm_tools_rs::compiler::sexp::decode_string;
use serde::{Deserialize, Serialize};
use serde_json;
use std::clone::Clone;
use std::fmt::Debug;

pub struct MessageBuffer<H> {
    pub handler: H,
}

struct MessageByteIter<'a> {
    buffers: &'a [Vec<u8>],
    pub cur: usize,
    pub byt: usize,
}

impl<'a> Iterator for MessageByteIter<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur >= self.buffers.len() {
            return None;
        }

        let cur_buf = &self.buffers[self.cur];
        if self.byt >= cur_buf.len() {
            self.cur += 1;
            self.byt = 0;
            return self.next();
        }

        let result = cur_buf[self.byt];
        self.byt += 1;
        Some(result)
    }
}

impl<H> MessageBuffer<H> {
    pub fn new(handler: H) -> MessageBuffer<H> {
        MessageBuffer { handler }
    }

    /// A generic message adapter which, given a message type M, presents an
    /// owned handler object of type H with an M each time one is decoded.
    ///
    /// For the debugger, dbg::handler::Debugger is the handler.
    pub fn process_message<M>(&mut self, msgdata: &[u8]) -> Result<Option<Vec<M>>, String>
    where
        H: MessageHandler<M>,
        for<'a> M: Serialize + Deserialize<'a> + Debug + Clone,
    {
        let msg_string = decode_string(msgdata);
        let as_json: serde_json::Value = serde_json::from_str(&msg_string)
            .map_err(|_| format!("failed to decode {msg_string}"))?;
        let msg: M = serde_json::from_value(as_json.clone())
            .map_err(|_| format!("failed to decode {msg_string}"))?;
        self.handler.handle_message(&as_json, &msg)
    }
}
