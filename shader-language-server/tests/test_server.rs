use core::panic;
use std::{
    env,
    io::{BufReader, Read},
    process::{Child, ChildStderr, ChildStdin, ChildStdout, Command, Stdio},
};

use lsp_types::{
    notification::{Exit, Initialized},
    request::{Initialize, Shutdown, WorkspaceConfiguration},
    InitializeParams, InitializedParams,
};

pub struct TestServer {
    child: Child,
    stdin: ChildStdin,
    reader: BufReader<ChildStdout>,
    err_reader: BufReader<ChildStderr>,
    request_id: i32,
}
impl TestServer {
    pub fn wasi() -> Option<TestServer> {
        use std::path::Path;

        use shader_sense::include::canonicalize;
        let server_path = canonicalize(Path::new(&format!(
            "../target/wasm32-wasip1-threads/debug/{}.{}",
            env!("CARGO_PKG_NAME").replace("_", "-"),
            "wasm"
        )))
        .unwrap();
        let test_folder = canonicalize(Path::new("../shader-sense/test")).unwrap();
        println!("Server path: {}", server_path.display());
        println!("Test folder: {}", test_folder.display());
        // If wasm is not built, simply skip the test.
        // On PC build workflow, no WASI available, too heavy to rebuild it, so skip instead.
        if !server_path.is_file() {
            println!("WASI server not built, skipping test.");
            return None;
        }
        assert!(test_folder.is_dir(), "Missing Test folder");
        let child = Command::new("wasmtime")
            .args([
                "--wasi",
                "threads=y",
                "--dir",
                format!("{}::/test", test_folder.display()).as_str(),
                format!("{}", server_path.display()).as_str(),
            ])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .env("RUST_LOG", "shader_language_server=trace")
            .spawn()
            .unwrap();
        Some(Self::from_child(child))
    }
    pub fn desktop() -> Option<TestServer> {
        use std::path::Path;

        use shader_sense::include::canonicalize;
        let server_path = canonicalize(Path::new(&format!(
            "../target/debug/{}{}",
            env!("CARGO_PKG_NAME").replace("_", "-"),
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
            .env("RUST_LOG", "shader_language_server=trace")
            .spawn()
            .unwrap();
        Some(Self::from_child(child))
    }
    fn from_child(mut child: Child) -> TestServer {
        let stdin = child.stdin.take().expect("Failed to open stdin");
        let stdout = child.stdout.take().expect("Failed to open stdout");
        let stderr = child.stderr.take().expect("Failed to open stdout");
        let reader = BufReader::new(stdout);
        let err_reader = BufReader::new(stderr);
        TestServer {
            child: child,
            request_id: 0,
            reader,
            err_reader,
            stdin,
        }
    }
    pub fn initialize(&mut self) {
        let params = InitializeParams::default();
        self.send_request::<Initialize>(&params, |_| {});
        self.send_notification::<Initialized>(&InitializedParams {});
        self.expect_request::<WorkspaceConfiguration>();
    }
    pub fn exit(&mut self) {
        self.send_request::<Shutdown>(&(), |_| {});
        self.send_notification::<Exit>(&());
        // Wait log for printing them.
        std::thread::sleep(std::time::Duration::from_micros(500));
        // Process seems to hang while joining threads. Kill it instead of waiting.
        self.child.kill().unwrap();
        // Print logs
        let mut errors = String::new();
        self.err_reader.read_to_string(&mut errors).unwrap();
        println!("stderr:\n{}", errors);
    }
    pub fn send_request<T: lsp_types::request::Request>(
        &mut self,
        params: &T::Params,
        callback: fn(T::Result),
    ) {
        let request = lsp_server::Message::Request(lsp_server::Request::new(
            lsp_server::RequestId::from(self.request_id),
            T::METHOD.into(),
            params,
        ));
        self.request_id += 1;
        println!("Send request: {}", serde_json::to_string(&request).unwrap());
        lsp_server::Message::write(request, &mut self.stdin).unwrap();
        // Wait for response
        loop {
            let message = lsp_server::Message::read(&mut self.reader).unwrap();
            println!("Received message: {:?}", message);
            match message {
                Some(message) => match message {
                    lsp_server::Message::Response(response) => {
                        match response.result {
                            Some(result) => {
                                let response: T::Result = serde_json::from_value(result).unwrap();
                                callback(response);
                            }
                            None => println!("No response received for request {}", T::METHOD),
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
                    let mut errors = String::new();
                    self.err_reader.read_to_string(&mut errors).unwrap();
                    panic!("Server crashed:\n{}", errors);
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
        lsp_server::Message::write(notification, &mut self.stdin).unwrap();
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
        lsp_server::Message::write(response, &mut self.stdin).unwrap();
    }
    fn expect_request<T: lsp_types::request::Request>(&mut self) {
        let message = lsp_server::Message::read(&mut self.reader).unwrap();
        println!("Received message: {:?}", message);
        match message.unwrap() {
            lsp_server::Message::Request(request) => {
                if request.method.as_str() == T::METHOD {
                    self.on_request(request);
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
    fn on_notification(&self, notification: lsp_server::Notification) {
        println!("Received notification {:?}", notification);
    }
    fn on_request(&mut self, request: lsp_server::Request) {
        match request.method.as_str() {
            "workspace/configuration" => self
                .send_response::<WorkspaceConfiguration>(request.id, vec![serde_json::Value::Null]),
            _ => {
                panic!("Unhandled request {}", request.method);
            }
        }
    }
}
