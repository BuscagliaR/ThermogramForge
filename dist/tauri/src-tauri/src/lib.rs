// ThermogramForge desktop shell.
//
// shinylive needs a service worker, and WKWebView refuses to register one on
// Tauri's custom `tauri://` scheme (the window stays blank). So instead of
// letting Tauri serve the bundle directly, we serve the embedded assets from
// a tiny HTTP server on 127.0.0.1 (a secure context, so service workers
// work) and point the window at it. Every response carries the COOP/COEP
// headers webR wants for SharedArrayBuffer.

use std::io::Cursor;
use std::net::TcpListener;
use std::thread;

use tauri::{AppHandle, WebviewUrl, WebviewWindowBuilder};
use tiny_http::{Header, Response, Server};

fn header(name: &str, value: &str) -> Header {
  Header::from_bytes(name.as_bytes(), value.as_bytes()).expect("valid header")
}

fn serve(app: AppHandle, listener: TcpListener) {
  let server = Server::from_listener(listener, None).expect("http server");
  for request in server.incoming_requests() {
    let mut path = request.url().split('?').next().unwrap_or("/").to_string();
    if path.ends_with('/') {
      path.push_str("index.html");
    }
    let resolver = app.asset_resolver();
    let asset = resolver
      .get(path.clone())
      .or_else(|| resolver.get(path.trim_start_matches('/').to_string()));

    let response = match asset {
      Some(asset) => {
        let bytes = asset.bytes().to_vec();
        let len = bytes.len();
        Response::new(200.into(), Vec::new(), Cursor::new(bytes), Some(len), None)
          .with_header(header("Content-Type", asset.mime_type()))
      }
      None => {
        let body = b"not found".to_vec();
        let len = body.len();
        Response::new(404.into(), Vec::new(), Cursor::new(body), Some(len), None)
          .with_header(header("Content-Type", "text/plain"))
      }
    };
    let response = response
      .with_header(header("Cross-Origin-Opener-Policy", "same-origin"))
      .with_header(header("Cross-Origin-Embedder-Policy", "require-corp"))
      .with_header(header("Cache-Control", "no-cache"));
    let _ = request.respond(response);
  }
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
  tauri::Builder::default()
    .setup(|app| {
      if cfg!(debug_assertions) {
        app.handle().plugin(
          tauri_plugin_log::Builder::default()
            .level(log::LevelFilter::Info)
            .build(),
        )?;
      }

      let listener = TcpListener::bind("127.0.0.1:0")?;
      let port = listener.local_addr()?.port();
      let handle = app.handle().clone();
      thread::spawn(move || serve(handle, listener));

      let url = format!("http://127.0.0.1:{port}/").parse()?;
      WebviewWindowBuilder::new(app, "main", WebviewUrl::External(url))
        .title("ThermogramForge")
        .inner_size(1280.0, 800.0)
        .min_inner_size(960.0, 600.0)
        .build()?;
      Ok(())
    })
    .run(tauri::generate_context!())
    .expect("error while building tauri application");
}
