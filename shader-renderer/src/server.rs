use std::net::SocketAddr;

use log::info;
use lsp_server::{Message, RequestId};

use crate::{
    renderer::Renderer,
    server::{
        connection::ServerConnection,
        error::ServerError,
        notification::{
            dispatch_notification, ErrorNotification, ErrorNotificationParams, ExitNotification,
            ResizeTargetNotification, UpdateShaderNotification,
        },
        request::{dispatch_request, RenderRequest, ShutdownRequest},
    },
};

mod connection;
mod error;
// pub for test
pub mod notification;
pub mod request;

#[derive(Debug)]
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
        info!("Starting shader renderer server with config {:?}", config);
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
        loop {
            let msg = match self.connection.connection.receiver.recv() {
                Ok(msg) => msg,
                Err(_) => {
                    // Client disconnected. The reader thread already reached the end of the
                    // transport, so joining it returns instead of blocking.
                    info!("Client disconnected, exiting server");
                    return Ok(self.connection.join()?);
                }
            };
            match msg {
                Message::Request(req) => {
                    if self.connection.connection.handle_shutdown(&req)? {
                        break; // Client asked to exit.
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
            }
        }
        return Ok(());
    }
    pub fn on_request(
        &mut self,
        request_id: RequestId,
        request: lsp_server::Request,
    ) -> Result<(), ServerError> {
        info!(
            "Received request #{} {} with data {}",
            request_id, request.method, request.params
        );
        dispatch_request!(self, request_id, request, [ShutdownRequest, RenderRequest,])
    }
    pub fn on_notification(
        &mut self,
        notification: lsp_server::Notification,
    ) -> Result<(), ServerError> {
        info!(
            "Received notification {} with data {}",
            notification.method, notification.params
        );
        dispatch_notification!(
            self,
            notification,
            [
                ExitNotification,
                ErrorNotification,
                ResizeTargetNotification,
                UpdateShaderNotification
            ]
        )
    }
    pub fn on_response(&mut self, response: lsp_server::Response) -> Result<(), ServerError> {
        // Here the callback return a delayed update
        info!(
            "Received response to request #{} with result {:?}",
            response.id, response.result
        );
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
