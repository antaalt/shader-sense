use core::panic;
use std::{
    collections::HashMap,
    env,
    io::{self, BufRead, BufReader, BufWriter, Read, Write},
    net::{TcpListener, TcpStream},
    path::Path,
    process::{Child, ChildStderr, ChildStdin, ChildStdout, Command, Stdio},
};

use lsp_types::{
    notification::{DidChangeConfiguration, Exit, Initialized},
    request::{Initialize, Request, Shutdown, WorkDoneProgressCreate, WorkspaceConfiguration},
    DidChangeConfigurationParams, InitializeParams, InitializedParams, Position,
    TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams, Url,
};
use serde_json::Value;
use shader_language_server::server::{server_config::ServerSerializedConfig, Transport};
use shader_sense::{include::canonicalize, shader::ShadingLanguage};

pub struct TestFile {
    pub url: Url,
    pub shading_language: ShadingLanguage,
    pub content: String,
}
impl TestFile {
    pub fn new(relative_path: &Path, shading_language: ShadingLanguage) -> Self {
        let file_path = canonicalize(relative_path).unwrap();
        let content = std::fs::read_to_string(&file_path).unwrap();
        let uri = Url::from_file_path(&file_path).unwrap();
        Self {
            url: uri,
            shading_language: shading_language,
            content: content,
        }
    }
    pub fn item(&self) -> TextDocumentItem {
        TextDocumentItem {
            uri: self.url.clone(),
            language_id: self.shading_language.to_string(),
            version: 0,
            text: self.content.clone(),
        }
    }
    pub fn identifier(&self) -> TextDocumentIdentifier {
        TextDocumentIdentifier {
            uri: self.url.clone(),
        }
    }
    #[allow(dead_code)]
    pub fn position_params(&self, line: u32, character: u32) -> TextDocumentPositionParams {
        TextDocumentPositionParams {
            text_document: self.identifier(),
            position: Position {
                line: line,
                character: character,
            },
        }
    }
}

struct StdioConnection {
    stdin: ChildStdin,
    reader: BufReader<ChildStdout>,
    err_reader: BufReader<ChildStderr>,
}

struct TcpConnection {
    _stream: TcpStream,
    reader: BufReader<TcpStream>,
    writer: BufWriter<TcpStream>,
    err_reader: BufReader<ChildStderr>,
}

enum Connection {
    Stdio(StdioConnection),
    Tcp(TcpConnection),
}

impl Connection {
    fn read_err_to_string(&mut self, string: &mut String) -> io::Result<usize> {
        match self {
            Connection::Stdio(stdio_connection) => {
                stdio_connection.err_reader.read_to_string(string)
            }
            Connection::Tcp(tcp_connection) => tcp_connection.err_reader.read_to_string(string),
        }
    }
    fn write(&mut self) -> &mut dyn Write {
        match self {
            Connection::Stdio(stdio_connection) => &mut stdio_connection.stdin,
            Connection::Tcp(tcp_connection) => &mut tcp_connection.writer,
        }
    }
    fn read(&mut self) -> &mut dyn BufRead {
        match self {
            Connection::Stdio(stdio_connection) => &mut stdio_connection.reader,
            Connection::Tcp(tcp_connection) => &mut tcp_connection.reader,
        }
    }
}

pub struct TestServer {
    child: Child,
    connection: Connection,
    request_id: i32,
    notification_handler: HashMap<&'static str, Box<dyn FnMut(Value)>>,
}
impl TestServer {
    pub fn new(config: ServerSerializedConfig, transport: Transport) -> Option<TestServer> {
        // Run the WASI server if required by env.
        let use_wasi_server = match std::env::var("USE_WASI_SERVER") {
            Ok(use_wasi_server) => use_wasi_server.parse().unwrap(),
            Err(_) => false,
        };
        if use_wasi_server {
            TestServer::wasi(config, transport)
        } else {
            TestServer::native(config, transport)
        }
    }
    fn wasi(config: ServerSerializedConfig, transport: Transport) -> Option<TestServer> {
        use std::path::Path;
        assert!(
            transport == Transport::Stdio,
            "Wasi server does not support socket"
        );
        use shader_sense::include::canonicalize;
        let server_path = canonicalize(Path::new(&format!(
            "../target/wasm32-wasip1-threads/debug/{}.{}",
            env!("CARGO_PKG_NAME").replace("_", "-"),
            "wasm"
        )))
        .unwrap();
        let test_folder = canonicalize(Path::new("../shader-sense/test")).unwrap();
        println!("Wasi server path: {}", server_path.display());
        println!("Test folder: {}", test_folder.display());
        // If wasm is not built, simply skip the test.
        // On PC build workflow, no WASI available, too heavy to rebuild it, so skip instead.
        if !server_path.is_file() {
            println!("WASI server not built, skipping test.");
            return None;
        }
        assert!(test_folder.is_dir(), "Missing Test folder");
        let serialized_config = serde_json::to_string(&config).unwrap();
        let child = Command::new("wasmtime")
            .args([
                "--wasm",
                "shared-memory=y",
                "--wasi",
                "threads=y",
                "--dir",
                format!("{}::/test", test_folder.display()).as_str(),
                format!("{}", server_path.display()).as_str(),
                "--config",
                &serialized_config,
            ])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .env("RUST_BACKTRACE", "full")
            .env("RUST_LOG", "shader_language_server=trace")
            .spawn()
            .unwrap();
        Some(Self::from_child(child, Transport::Stdio))
    }
    fn native(config: ServerSerializedConfig, transport: Transport) -> Option<TestServer> {
        use std::path::Path;

        use shader_sense::include::canonicalize;
        let server_path = canonicalize(Path::new(&format!(
            "../target/debug/{}{}",
            env!("CARGO_PKG_NAME").replace("_", "-"),
            std::env::consts::EXE_SUFFIX
        )))
        .unwrap();
        let test_folder = canonicalize(Path::new("../shader-sense/test")).unwrap();
        println!("Native server path: {}", server_path.display());
        println!("Test folder: {}", test_folder.display());
        // If server is not built, simply skip the test.
        if !server_path.is_file() {
            println!("Native server not built, skipping test.");
            return None;
        }
        assert!(test_folder.is_dir(), "Missing Test folder");
        let serialized_config = serde_json::to_string(&config).unwrap();
        let (transport_parameter, transport_arg) = match transport {
            Transport::Stdio => ("--stdio", "".to_string()),
            Transport::TcpListen(socket_addr) => ("--tcp-listen", socket_addr.to_string()),
            Transport::TcpConnect(socket_addr) => ("--tcp-connect", socket_addr.to_string()),
        };
        let child = Command::new(server_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .env("RUST_BACKTRACE", "full")
            .env("RUST_LOG", "shader_language_server=trace")
            .args([
                "--config",
                &serialized_config,
                transport_parameter,
                &transport_arg,
            ])
            .spawn()
            .unwrap();
        Some(Self::from_child(child, transport))
    }
    fn from_child(mut child: Child, transport: Transport) -> TestServer {
        let connection = match transport {
            Transport::Stdio => {
                let stdin = child.stdin.take().expect("Failed to open stdin");
                let stdout = child.stdout.take().expect("Failed to open stdout");
                let stderr = child.stderr.take().expect("Failed to open stdout");
                let reader = BufReader::new(stdout);
                let err_reader = BufReader::new(stderr);
                Connection::Stdio(StdioConnection {
                    reader,
                    err_reader,
                    stdin,
                })
            }
            Transport::TcpListen(socket_addr) => {
                let stream = TcpStream::connect(socket_addr).unwrap();
                let stderr = child.stderr.take().expect("Failed to open stdout");
                let writer = BufWriter::new(stream.try_clone().unwrap());
                let reader = BufReader::new(stream.try_clone().unwrap());
                let err_reader = BufReader::new(stderr);
                Connection::Tcp(TcpConnection {
                    _stream: stream,
                    reader,
                    writer,
                    err_reader,
                })
            }
            Transport::TcpConnect(socket_addr) => {
                let listener = TcpListener::bind(socket_addr).unwrap();
                let stderr = child.stderr.take().expect("Failed to open stdout");
                let (stream, _addr) = listener.accept().unwrap();
                let writer = BufWriter::new(stream.try_clone().unwrap());
                let reader = BufReader::new(stream.try_clone().unwrap());
                let err_reader = BufReader::new(stderr);
                Connection::Tcp(TcpConnection {
                    _stream: stream,
                    reader,
                    writer,
                    err_reader,
                })
            }
        };
        let mut server = TestServer {
            child: child,
            connection,
            request_id: 0,
            notification_handler: HashMap::new(),
        };
        // Send an LSP initialize request
        server.initialize();
        server
    }
    fn initialize(&mut self) {
        let params = InitializeParams::default();
        self.send_request::<Initialize>(&params, |result| {
            // Validate params for all test.
            let result = result.unwrap();
            let server_info = result.server_info.unwrap();
            assert!(
                server_info.name == "shader-language-server",
                "Unexpected server name {}",
                server_info.name
            );
            assert!(server_info.version.unwrap() == env!("CARGO_PKG_VERSION"));
        });
        self.send_notification::<Initialized>(&InitializedParams {});
    }
    fn get_server_stderr(&mut self) -> io::Result<String> {
        let mut errors = String::new();
        self.connection.read_err_to_string(&mut errors)?;
        Ok(errors)
    }
    fn exit(&mut self) {
        self.send_request::<Shutdown>(&(), |_| {});
        self.send_notification::<Exit>(&());
        // Wait log for printing them.
        std::thread::sleep(std::time::Duration::from_micros(500));
        // Process seems to hang while joining threads. Kill it instead of waiting.
        self.child.kill().unwrap();
        // Print logs
        println!("stderr:\n{}", self.get_server_stderr().unwrap());
    }
    fn kill(&mut self) {
        // Kill before reading stderr to correctly output EOF for read_to_string.
        self.child.kill().unwrap();
        // Avoid crashing here as we might be panicking
        match self.get_server_stderr() {
            Ok(logs) => println!("Panic stderr:\n{}", logs),
            Err(err) => println!("Failed to get server log while unwinding panic: {}", err),
        }
    }
    pub fn send_request<T: lsp_types::request::Request>(
        &mut self,
        params: &T::Params,
        callback: fn(Option<T::Result>),
    ) {
        let request = lsp_server::Message::Request(lsp_server::Request::new(
            lsp_server::RequestId::from(self.request_id),
            T::METHOD.into(),
            params,
        ));
        self.request_id += 1;
        println!("Send request: {}", serde_json::to_string(&request).unwrap());
        lsp_server::Message::write(request, &mut self.connection.write()).unwrap();
        // Wait for response
        loop {
            let message = lsp_server::Message::read(&mut self.connection.read()).unwrap();
            println!("Received message: {:?}", message);
            match message {
                Some(message) => match message {
                    lsp_server::Message::Response(response) => {
                        match response.result {
                            Some(result) => {
                                let response: T::Result = serde_json::from_value(result).unwrap();
                                callback(Some(response));
                            }
                            None => callback(None),
                        }
                        break;
                    }
                    // Handle other messages.
                    lsp_server::Message::Notification(notification) => {
                        self.on_notification(notification)
                    }
                    lsp_server::Message::Request(request) => self.on_request(request),
                },
                None => {
                    panic!("Server crashed:\n{}", self.get_server_stderr().unwrap());
                }
            }
        }
    }
    pub fn send_notification<T: lsp_types::notification::Notification>(
        &mut self,
        params: &T::Params,
    ) {
        let notification = lsp_server::Message::Notification(lsp_server::Notification::new(
            T::METHOD.into(),
            params,
        ));
        println!(
            "Send notification: {}",
            serde_json::to_string(&notification).unwrap()
        );
        lsp_server::Message::write(notification, &mut self.connection.write()).unwrap();
    }
    pub fn send_response<T: lsp_types::request::Request>(
        &mut self,
        req_id: lsp_server::RequestId,
        result: T::Result,
    ) {
        let response = lsp_server::Message::Response(lsp_server::Response::new_ok(req_id, result));
        println!(
            "Send response: {}",
            serde_json::to_string(&response).unwrap()
        );
        lsp_server::Message::write(response, &mut self.connection.write()).unwrap();
    }
    #[allow(dead_code)]
    pub fn update_configuration(&mut self, json: serde_json::Value) {
        self.send_notification::<DidChangeConfiguration>(&DidChangeConfigurationParams {
            settings: Value::Null, // Unused
        });
        self.expect_request::<WorkspaceConfiguration>(vec![json]);
    }
    fn expect_request<T: lsp_types::request::Request>(&mut self, response: T::Result) {
        let message = lsp_server::Message::read(&mut self.connection.read()).unwrap();
        println!("Received message: {:?}", message);
        match message.unwrap() {
            lsp_server::Message::Request(request) => {
                if request.method.as_str() == T::METHOD {
                    self.send_response::<T>(request.id, response);
                } else {
                    panic!(
                        "Expected request {}, received request {}",
                        T::METHOD,
                        request.method
                    );
                }
            }
            message => panic!("Expected request {}, received {:?}", T::METHOD, message),
        }
    }
    fn on_notification(&mut self, notification: lsp_server::Notification) {
        println!("Received notification {:?}", notification);
        match self
            .notification_handler
            .get_mut(notification.method.as_str())
        {
            Some(handler) => (handler)(notification.params),
            None => {}
        }
    }
    fn on_request(&mut self, request: lsp_server::Request) {
        match request.method.as_str() {
            WorkspaceConfiguration::METHOD => self
                .send_response::<WorkspaceConfiguration>(request.id, vec![serde_json::Value::Null]),
            WorkDoneProgressCreate::METHOD => {
                self.send_response::<WorkDoneProgressCreate>(request.id, ())
            }
            _ => {
                panic!("Unhandled request {}", request.method);
            }
        }
    }
    #[allow(dead_code)]
    pub fn subscribe<T: lsp_types::notification::Notification, F: FnMut(Value) + 'static>(
        &mut self,
        callback: F,
    ) {
        self.notification_handler
            .insert(T::METHOD, Box::new(callback));
    }
}

impl Drop for TestServer {
    fn drop(&mut self) {
        if std::thread::panicking() {
            self.kill();
        } else {
            self.exit();
        }
    }
}
