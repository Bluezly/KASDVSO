
use crate::types::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::net::TcpStream;
use std::sync::{
    atomic::{AtomicI64, Ordering},
    mpsc,
    Arc, Mutex,
};
use std::thread;
use std::time::Duration;

use tungstenite::{connect, Message};
use tungstenite::stream::MaybeTlsStream;



pub fn create_ws_module() -> NativeModuleInstance {
    let mut functions = HashMap::new();

    register_function(&mut functions, "connect", ws_connect);
    register_function(&mut functions, "send", ws_send);
    register_function(&mut functions, "recv", ws_recv);
    register_function(&mut functions, "close", ws_close);
    register_function(&mut functions, "is_open", ws_is_open);
    register_function(&mut functions, "last_error", ws_last_error);

    let lib = unsafe {
        libloading::Library::new("kernel32.dll")
            .expect("Failed to load kernel32.dll as native handle")
    };

    NativeModuleInstance {
        module_name: "ws".to_string(),
        library_handle: Arc::new(lib),
        exported_functions: functions,
    }
}

static NEXT_ID: AtomicI64 = AtomicI64::new(1);

#[derive(Debug)]
enum WsCmd {
    SendText(String),
    Close,
}

#[derive(Debug)]
struct WsConn {
    tx: mpsc::Sender<WsCmd>,
    rx: mpsc::Receiver<String>,
    open: Arc<Mutex<bool>>,
}

lazy_static::lazy_static! {
    static ref CONNS: Mutex<HashMap<i64, WsConn>> = Mutex::new(HashMap::new());
    static ref LAST_ERROR: Mutex<String> = Mutex::new(String::new());
}

thread_local! {
    static LAST_STR: RefCell<Vec<u8>> = RefCell::new(Vec::new());
}

fn set_last_error(s: impl Into<String>) {
    *LAST_ERROR.lock().unwrap() = s.into();
}

fn get_last_error() -> String {
    LAST_ERROR.lock().unwrap().clone()
}


fn register_function(
    map: &mut HashMap<String, NativeFunction>,
    name: &str,
    func: extern "C" fn(usize, *const NativeArgument) -> NativeReturn,
) {
    let name_bytes = name.as_bytes();
    map.insert(
        name.to_string(),
        NativeFunction {
            name_pointer: name_bytes.as_ptr(),
            name_length: name_bytes.len(),
            function_pointer: func,
        },
    );
}

fn return_int(v: i64) -> NativeReturn {
    NativeReturn {
        return_type: 0,
        integer_value: v,
        string_pointer: std::ptr::null(),
        string_length: 0,
    }
}

fn return_string(s: &str) -> NativeReturn {
    LAST_STR.with(|cell| {
        let mut buf = cell.borrow_mut();
        buf.clear();
        buf.extend_from_slice(s.as_bytes());

        NativeReturn {
            return_type: 1,
            integer_value: 0,
            string_pointer: buf.as_ptr(),
            string_length: buf.len(),
        }
    })
}

fn error_string(msg: &str) -> NativeReturn {
    set_last_error(msg);
    return_string(msg)
}


unsafe fn get_str_arg(arg: &NativeArgument) -> Option<String> {
    if arg.argument_type != 1 || arg.string_pointer.is_null() {
        return None;
    }
    let bytes = std::slice::from_raw_parts(arg.string_pointer, arg.string_length);
    Some(String::from_utf8_lossy(bytes).to_string())
}

unsafe fn get_int_arg(arg: &NativeArgument) -> Option<i64> {
    if arg.argument_type != 0 {
        return None;
    }
    Some(arg.integer_value)
}


fn set_stream_timeouts(stream: &mut MaybeTlsStream<TcpStream>) {
    let to = Some(Duration::from_millis(50));

    if let MaybeTlsStream::Plain(tcp) = stream {
        let _ = tcp.set_read_timeout(to);
        let _ = tcp.set_write_timeout(to);
    }
}

extern "C" fn ws_connect(argc: usize, argv: *const NativeArgument) -> NativeReturn {
    if argc != 1 {
        return error_string("ws.connect expects 1 argument: url (string)");
    }

    let url = unsafe { get_str_arg(&*argv) };
    let Some(url) = url else {
        return error_string("ws.connect url must be a string");
    };

    let id = NEXT_ID.fetch_add(1, Ordering::SeqCst);

    let (cmd_tx, cmd_rx) = mpsc::channel::<WsCmd>();
    let (msg_tx, msg_rx) = mpsc::channel::<String>();
    let open_flag = Arc::new(Mutex::new(true));
    let open_flag_thread = open_flag.clone();

    thread::spawn(move || {
        let (mut socket, _resp) = match connect(url.as_str()) {
            Ok(pair) => pair,
            Err(e) => {
                let _ = msg_tx.send(format!("[ws] connect failed: {e}"));
                *open_flag_thread.lock().unwrap() = false;
                return;
            }
        };

        set_stream_timeouts(socket.get_mut());

        loop {
            match cmd_rx.try_recv() {
                Ok(WsCmd::SendText(text)) => {
                    if let Err(e) = socket.send(Message::Text(text)) {
                        let _ = msg_tx.send(format!("[ws] send failed: {e}"));
                        break;
                    }
                }
                Ok(WsCmd::Close) => {
                    let _ = socket.close(None);
                    break;
                }
                Err(mpsc::TryRecvError::Empty) => {}
                Err(_) => break,
            }

            match socket.read() {
                Ok(Message::Text(t)) => {
                    let _ = msg_tx.send(t);
                }
                Ok(Message::Binary(_b)) => {
                    let _ = msg_tx.send("[ws] (binary ignored)".to_string());
                }
                Ok(Message::Ping(p)) => {
                    let _ = socket.send(Message::Pong(p));
                }
                Ok(Message::Pong(_)) => {}
                Ok(Message::Close(_)) => break,

                Ok(Message::Frame(_)) => {
                    // ignore
                }

                Err(tungstenite::Error::Io(ref io))
                    if io.kind() == std::io::ErrorKind::WouldBlock
                        || io.kind() == std::io::ErrorKind::TimedOut =>
                {
                }

                Err(e) => {
                    let _ = msg_tx.send(format!("[ws] recv failed: {e}"));
                    break;
                }
            }
        }

        *open_flag_thread.lock().unwrap() = false;
    });

    CONNS.lock().unwrap().insert(
        id,
        WsConn {
            tx: cmd_tx,
            rx: msg_rx,
            open: open_flag,
        },
    );

    return_int(id)
}

extern "C" fn ws_send(argc: usize, argv: *const NativeArgument) -> NativeReturn {
    if argc != 2 {
        return error_string("ws.send expects 2 arguments: handle (int), text (string)");
    }

    unsafe {
        let args = std::slice::from_raw_parts(argv, 2);
        let Some(id) = get_int_arg(&args[0]) else {
            return error_string("ws.send handle must be int");
        };
        let Some(text) = get_str_arg(&args[1]) else {
            return error_string("ws.send text must be string");
        };

        let conns = CONNS.lock().unwrap();
        let Some(conn) = conns.get(&id) else {
            return error_string("ws.send unknown handle");
        };

        if !*conn.open.lock().unwrap() {
            return error_string("ws.send connection is closed");
        }

        if conn.tx.send(WsCmd::SendText(text)).is_err() {
            return error_string("ws.send failed to enqueue send");
        }

        return_int(1)
    }
}

extern "C" fn ws_recv(argc: usize, argv: *const NativeArgument) -> NativeReturn {
    if argc != 1 {
        return error_string("ws.recv expects 1 argument: handle (int)");
    }

    unsafe {
        let Some(id) = get_int_arg(&*argv) else {
            return error_string("ws.recv handle must be int");
        };

        let conns = CONNS.lock().unwrap();
        let Some(conn) = conns.get(&id) else {
            return error_string("ws.recv unknown handle");
        };

        match conn.rx.try_recv() {
            Ok(msg) => return_string(msg.as_str()),
            Err(mpsc::TryRecvError::Empty) => return_string(""),
            Err(_) => return_string(""),
        }
    }
}

extern "C" fn ws_close(argc: usize, argv: *const NativeArgument) -> NativeReturn {
    if argc != 1 {
        return error_string("ws.close expects 1 argument: handle (int)");
    }

    unsafe {
        let Some(id) = get_int_arg(&*argv) else {
            return error_string("ws.close handle must be int");
        };

        let mut conns = CONNS.lock().unwrap();
        if let Some(conn) = conns.remove(&id) {
            let _ = conn.tx.send(WsCmd::Close);
            *conn.open.lock().unwrap() = false;
        }

        return_int(1)
    }
}

extern "C" fn ws_is_open(argc: usize, argv: *const NativeArgument) -> NativeReturn {
    if argc != 1 {
        return error_string("ws.is_open expects 1 argument: handle (int)");
    }

    unsafe {
        let Some(id) = get_int_arg(&*argv) else {
            return error_string("ws.is_open handle must be int");
        };

        let conns = CONNS.lock().unwrap();
        let Some(conn) = conns.get(&id) else {
            return return_int(0);
        };

        let open = *conn.open.lock().unwrap();
        return_int(if open { 1 } else { 0 })
    }
}

extern "C" fn ws_last_error(argc: usize, _argv: *const NativeArgument) -> NativeReturn {
    if argc != 0 {
        return error_string("ws.last_error expects 0 arguments");
    }
    let s = get_last_error();
    return_string(&s)
}
