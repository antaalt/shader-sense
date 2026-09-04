use core::panic;
use std::{
    collections::HashMap,
    env,
    io::{self, BufRead, BufReader, Read, Write},
    path::{Path, PathBuf},
    process::{Child, ChildStderr, ChildStdin, ChildStdout, Command, Stdio},
};

use lsp_server::ResponseError;
use serde_json::Value;
use shader_renderer::server::{self};
use shader_sense::{include::canonicalize, shader::ShadingLanguage};

pub struct TestFile {
    pub file_path: PathBuf,
    pub shading_language: ShadingLanguage,
    pub content: String,
}
impl TestFile {
    pub fn new(relative_path: &Path, shading_language: ShadingLanguage) -> Self {
        let file_path = canonicalize(relative_path).unwrap();
        let content = std::fs::read_to_string(&file_path).unwrap();
        Self {
            file_path,
            shading_language: shading_language,
            content: content,
        }
    }
}
struct Connection {
    stdin: ChildStdin,
    reader: BufReader<ChildStdout>,
    err_reader: BufReader<ChildStderr>,
}

impl Connection {
    fn read_err_to_string(&mut self, string: &mut String) -> io::Result<usize> {
        self.err_reader.read_to_string(string)
    }
    fn write(&mut self) -> &mut dyn Write {
        &mut self.stdin
    }
    fn read(&mut self) -> &mut dyn BufRead {
        &mut self.reader
    }
}

pub struct TestServer {
    child: Child,
    connection: Connection,
    request_id: i32,
    notification_handler: HashMap<&'static str, Box<dyn FnMut(Value)>>,
}
impl TestServer {
    pub fn desktop() -> Option<TestServer> {
        use std::path::Path;

        use shader_sense::include::canonicalize;
        let server_path = canonicalize(Path::new(&format!(
            "../target/debug/{}{}",
            env!("CARGO_PKG_NAME"),
            std::env::consts::EXE_SUFFIX
        )))
        .unwrap();
        let test_folder = canonicalize(Path::new("../shader-sense/test")).unwrap();
        println!("Server path: {}", server_path.display());
        println!("Test folder: {}", test_folder.display());
        // If server is not built, simply skip the test.
        if !server_path.is_file() {
            println!("Desktop server not built, skipping test.");
            return None;
        }
        assert!(test_folder.is_dir(), "Missing Test folder");
        let child = Command::new(server_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .env("RUST_BACKTRACE", "full")
            .env("RUST_LOG", "shader_renderer=trace,naga=info,wgpu=info") // Somehow naga=debug hang the process...
            .args(["--stdio"])
            .spawn()
            .unwrap();
        Some(Self::from_child(child))
    }
    fn from_child(mut child: Child) -> TestServer {
        let stdin = child.stdin.take().expect("Failed to open stdin");
        let stdout = child.stdout.take().expect("Failed to open stdout");
        let stderr = child.stderr.take().expect("Failed to open stderr");
        let reader = BufReader::new(stdout);
        let err_reader = BufReader::new(stderr);
        let connection = Connection {
            reader,
            err_reader,
            stdin,
        };
        TestServer {
            child: child,
            connection,
            request_id: 0,
            notification_handler: HashMap::new(),
        }
    }
    fn get_server_stderr(&mut self) -> io::Result<String> {
        let mut errors = String::new();
        self.connection.read_err_to_string(&mut errors)?;
        Ok(errors)
    }
    fn exit(&mut self) {
        self.send_request::<server::request::ShutdownRequest>(&(), |_| {});
        self.send_notification::<server::notification::ExitNotification>(&());
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
    pub fn send_request<T: server::request::Request>(
        &mut self,
        params: &T::Params,
        callback: fn(T::Result),
    ) {
        self.send_request_error::<T>(params, callback, |error| {
            assert!(false, "Error in request {:?}", error);
        });
    }
    pub fn send_request_error<T: server::request::Request>(
        &mut self,
        params: &T::Params,
        callback: fn(T::Result),
        error_callback: fn(ResponseError),
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
                                callback(response);
                            }
                            None => match response.error {
                                Some(error) => error_callback(error),
                                None => {} // Neither error nor ok
                            },
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
    pub fn send_notification<T: server::notification::Notification>(&mut self, params: &T::Params) {
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
    pub fn send_response<T: server::request::Request>(
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
    fn expect_request<T: server::request::Request>(&mut self, response: T::Result) {
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
        println!("Received request {:?}", request.method);
        match request.method.as_str() {
            _ => {
                panic!("Unhandled request {}", request.method);
            }
        }
    }
    #[allow(dead_code)]
    pub fn subscribe<T: server::notification::Notification, F: FnMut(Value) + 'static>(
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
