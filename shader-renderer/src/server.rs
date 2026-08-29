use std::net::SocketAddr;

use lsp_server::{Message, RequestId};

use crate::{
    renderer::Renderer,
    server::{
        connection::ServerConnection,
        error::ServerError,
        notification::{
            dispatch_notification, ErrorNotification, ErrorNotificationParams,
            ResizeTargetNotification,
        },
        request::{dispatch_request, ShutdownRequest},
    },
};

mod connection;
mod error;
mod notification;
mod request;

pub struct ServerConfig {
    pub width: u32,
    pub height: u32,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            width: 1280,
            height: 720,
        }
    }
}

pub struct Server {
    renderer: Renderer,
    config: ServerConfig,
    connection: ServerConnection,
}

#[derive(Default)]
pub enum ServerTransport {
    #[default]
    Stdio,
    TcpListen(SocketAddr),
    TcpConnect(SocketAddr),
}

impl Server {
    pub fn new(config: ServerConfig, transport: ServerTransport) -> Result<Self, ServerError> {
        Ok(Self {
            renderer: Renderer::new(config.width, config.height),
            config,
            connection: match transport {
                ServerTransport::Stdio => ServerConnection::stdio(),
                ServerTransport::TcpConnect(addr) => ServerConnection::connect(addr)?,
                ServerTransport::TcpListen(addr) => ServerConnection::listen(addr)?,
            },
        })
    }
    pub fn run(&mut self) -> Result<(), ServerError> {
        let msg_err = self.connection.connection.receiver.recv();
        match msg_err {
            Ok(msg) => match msg {
                Message::Request(req) => {
                    if self.connection.connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    let id = req.id.clone();
                    match self.on_request(id.clone(), req) {
                        Ok(_) => {}
                        Err(err) => self.connection.send_response_error(
                            id,
                            lsp_server::ErrorCode::InternalError,
                            err.to_string(),
                        ),
                    }
                }
                Message::Response(response) => match self.on_response(response) {
                    Ok(_) => {}
                    Err(err) => self.connection.send_notification::<ErrorNotification>(
                        ErrorNotificationParams {
                            message: err.to_string(),
                        },
                    ),
                },
                Message::Notification(notification) => match self.on_notification(notification) {
                    Ok(_) => {}
                    Err(err) => self.connection.send_notification::<ErrorNotification>(
                        ErrorNotificationParams {
                            message: err.to_string(),
                        },
                    ),
                },
            },
            Err(_) => {
                return Ok(()); // Exit server.
            }
        }
        return Ok(());
    }
    pub fn on_request(
        &mut self,
        request_id: RequestId,
        request: lsp_server::Request,
    ) -> Result<(), ServerError> {
        dispatch_request!(self, request_id, request, [ShutdownRequest,])
    }
    pub fn on_notification(
        &mut self,
        notification: lsp_server::Notification,
    ) -> Result<(), ServerError> {
        dispatch_notification!(
            self,
            notification,
            [ErrorNotification, ResizeTargetNotification]
        )
    }
    pub fn on_response(&mut self, response: lsp_server::Response) -> Result<(), ServerError> {
        // Here the callback return a delayed update
        match self.connection.remove_callback(&response.id) {
            Some(callback) => match response.result {
                Some(result) => callback(self, result),
                None => Ok(()), // Received message can be empty.
            },
            None => Err(ServerError::InternalError(format!(
                "Received unhandled response: {:#?}",
                response
            ))),
        }
    }
}
