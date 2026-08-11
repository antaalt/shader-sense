use std::{collections::HashMap, net::ToSocketAddrs};

use log::error;
use lsp_server::{Connection, IoThreads, Message, RequestId, Response};
use lsp_types::{InitializeParams, MessageType, ShowMessageParams};
use serde_json::Value;

use crate::server::{async_message::AsyncMessage, common::ServerLanguageError};

use super::ServerLanguage;

pub struct ServerConnection {
    pub connection: Connection,
    io_threads: Option<IoThreads>,
    request_id: i32,
    request_callbacks: HashMap<
        RequestId,
        fn(&mut ServerLanguage, Value) -> Result<AsyncMessage, ServerLanguageError>,
    >,
}

impl ServerConnection {
    pub fn stdio() -> Self {
        // Create the transport. Includes the stdio (stdin and stdout) versions but this could
        // also be implemented to use sockets or HTTP.
        let (connection, io_threads) = Connection::stdio();
        Self {
            connection,
            io_threads: Some(io_threads),
            request_id: 0,
            request_callbacks: HashMap::new(),
        }
    }
    pub fn listen<A: ToSocketAddrs>(addr: A) -> std::io::Result<Self> {
        // Create the transport. Includes the stdio (stdin and stdout) versions but this could
        // also be implemented to use sockets or HTTP.
        let (connection, io_threads) = Connection::listen(addr)?;
        Ok(Self {
            connection,
            io_threads: Some(io_threads),
            request_id: 0,
            request_callbacks: HashMap::new(),
        })
    }
    pub fn connect<A: ToSocketAddrs>(addr: A) -> std::io::Result<Self> {
        // Create the transport. Includes the stdio (stdin and stdout) versions but this could
        // also be implemented to use sockets or HTTP.
        let (connection, io_threads) = Connection::connect(addr)?;
        Ok(Self {
            connection,
            io_threads: Some(io_threads),
            request_id: 0,
            request_callbacks: HashMap::new(),
        })
    }
    pub fn initialize(
        &mut self,
        server_capabilities: Value,
    ) -> Result<InitializeParams, Box<dyn std::error::Error + Sync + Send>> {
        match self.connection.initialize(server_capabilities) {
            Ok(initialization_params) => {
                let client_initialization_params: InitializeParams =
                    serde_json::from_value(initialization_params)?;
                Ok(client_initialization_params)
            }
            Err(e) => {
                if e.channel_is_disconnected() {
                    self.io_threads.take().unwrap().join()?;
                }
                Err(e.into())
            }
        }
    }
    pub fn remove_callback(
        &mut self,
        request_id: &RequestId,
    ) -> Option<fn(&mut ServerLanguage, Value) -> Result<AsyncMessage, ServerLanguageError>> {
        self.request_callbacks.remove(request_id)
    }
    pub fn send_response<N: lsp_types::request::Request>(
        &self,
        request_id: RequestId,
        params: N::Result,
    ) {
        let response = Response::new_ok::<N::Result>(request_id, params);
        self.send(response.into());
    }
    pub fn send_response_error(
        &self,
        request_id: RequestId,
        code: lsp_server::ErrorCode,
        message: String,
    ) {
        let response = Response::new_err(request_id, code as i32, message);
        self.send(response.into());
    }
    pub fn send_notification<N: lsp_types::notification::Notification>(&self, params: N::Params) {
        let not = lsp_server::Notification::new(N::METHOD.to_owned(), params);
        self.send(not.into());
    }
    pub fn send_notification_error(&self, message: String) {
        error!("NOTIFICATION: {}", message);
        self.send_notification::<lsp_types::notification::ShowMessage>(ShowMessageParams {
            typ: MessageType::ERROR,
            message: message,
        })
    }
    pub fn send_request<R: lsp_types::request::Request>(
        &mut self,
        params: R::Params,
        callback: fn(&mut ServerLanguage, Value) -> Result<AsyncMessage, ServerLanguageError>,
    ) {
        let request_id = RequestId::from(self.request_id);
        self.request_id = self.request_id + 1;
        self.request_callbacks.insert(request_id.clone(), callback);
        let req = lsp_server::Request::new(request_id, R::METHOD.to_owned(), params);
        self.send(req.into());
    }
    fn send(&self, message: Message) {
        self.connection
            .sender
            .send(message)
            .expect("Failed to send a message");
    }

    pub fn join(&mut self) -> std::io::Result<()> {
        match self.io_threads.take() {
            Some(h) => h.join(),
            None => Ok(()),
        }
    }
}
