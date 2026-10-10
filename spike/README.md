# Phase 0 spike — results log

Branch `v2`. Plan: `~/.claude/plans/read-known-issues-md-news-md-and-serene-bear.md` (Phase 0 section).

## Status

| Goal | State |
|---|---|
| G1 vectorised endpoint detection bit-identical to V1 | **done** — 2,499 expectations pass across 62 oracle files |
| G2 V1 oracle committed with version stamps | **done** — `core/tests/testthat/fixtures/oracle/` (12 MB) |
| G3 core builds for webR; browser timings | in progress |
| G4 Tauri shell runs the bundle offline on macOS | scaffolded (`dist/tauri`), not yet built |
| G5 skeleton + CI | done locally; CI workflows written, not yet run |

## What the oracle showed

- The V1 port (`ep_rolling_sd_v1`) reproduces V1's endpoints, signal flags and
  interpolated curves **exactly** on every fixture × parameter set that V1
  can process (6 param sets; `w120` and `excl55_85` fail in V1 on the
  451-point sample files, so those oracle files are empty and the tests skip).
- The vectorised default (`ep_rolling_sd`) reproduces the port window by
  window (identical SD profiles, identical chosen windows).
- `sig_kpss` agrees with `sig_auto_arima_v1` on every sample.
- V1 itself is **not** bit-reproducible across R sessions: the app-written
  `Small_RDS.rds` (R 4.5.1, 2025-10-23) differs from today's V1 re-run
  (R 4.5.2) at ~1e-15 in the curves, with identical endpoints/flags. That is
  the empirical basis for the "equivalent within tolerance + frozen
  reference" policy.
- **Across platforms the gap is larger.** The first CI run (x86_64 Linux,
  R 4.5.3) reproduced every endpoint and signal flag exactly but showed
  curve differences up to ~2e-7 dCp on the short-segment parameter sets
  (`outmost`, `mid`, `w120`), and 1-ulp differences in the *parsed input*
  (base `read.csv` is not correctly rounded on every platform; the oracle
  was parsed by readr on macOS). `smooth.spline(cv = TRUE)`'s lambda search
  amplifies ulp-level input changes. Test policy is therefore: bit-identical
  on the oracle's recorded platform, otherwise an absolute bound of 1e-5 dCp
  on curves and 1e-9 on parsed inputs (`core/tests/testthat/helper-fixtures.R`).
  Consequence for the Cancers replication vignette: state the platform, or
  expect agreement to ~1e-5 rather than to the bit.

## Native timings (Apple Silicon, R 4.5.2)

Idle machine, `Rscript spike/timing/bench_native.R`, 2026-10-09. All samples
are at instrument density (~5,390 points per sample).

| run | samples | wall | endpoints+baseline | interpolate | signal | per sample |
|---|---|---|---|---|---|---|
| Small, default (`rolling_sd` + `kpss`) | 20 | 0.85 s | 0.70 s | 0.10 s | 0.04 s | 43 ms |
| Small, reference (`rolling_sd_v1` + `auto_arima_v1`) | 20 | 5.12 s | 0.89 s | 0.09 s | 4.13 s | 256 ms |
| Large500, default | 496 | 19.3 s | 16.4 s | 2.4 s | 0.4 s | 39 ms |

Reading `Large500.csv` (26 MB wide CSV) with `read.csv`: 8.7 s — worth a
`data.table::fread` / `vroom` comparison in Phase 1 (this is I/O, where those
packages do help).

For scale: the V1 oracle capture (same samples, V1 code path) ran at roughly
**9 s per sample** at instrument density because `endpoint.detection` grows a
data.frame with `rbind` per window and is called twice. The default path above
is ~40 ms per sample.

`auto.arima` vs KPSS: ~200 ms vs ~3.5 ms per sample for the identical decision.

## webR / shinylive

Measured 2026-10-09 (Apple Silicon; Chrome = V8, Tauri = WKWebView/JavaScriptCore).
`thermogramforge.core` is bundled into shinylive's own package set by
`spike/build_site.R` (runtime `webr::install()` from a side-car repo hung
silently and was abandoned).

| metric | threshold (pass / fail) | Chrome | Tauri app |
|---|---|---|---|
| core package loaded after UI appears | — | 1.7 s | 3.8 s |
| 20 samples end-to-end (Small.csv) | ≤ 20 s / > 60 s | **4.3 s** | **8.0 s** |
| wasm / native ratio (20 samples, native 0.85 s) | ≤ 4× / > 8× | 5.1× (yellow) | 9.4× (over) |
| 500 samples (Large500.csv, 496 samples, 58 MB upload) | ≤ 5 min / > 12 min | **107 s (1.8 min)** | **211 s (3.5 min)** |
| wasm / native ratio (500 samples, native 19.3 s) | ≤ 4× / > 8× | 5.5× (yellow) | 11× (over) |
| peak tab memory during the 500-sample run | — | 731 MB | — |
| bundle size (site) | ≤ 150 MB / > 300 MB | **90.2 MB** | 85 MB `.app` |

20-sample stage breakdown (seconds): Chrome read 0.26 / endpoints+baseline
4.05 / interpolate 0.16 / signal 0.07; Tauri 1.38 / 7.62 / 0.18 / 0.16.
500-sample: Chrome read 10.4 / 100.8 / 3.8 / 1.7; Tauri 10.1 / 201.3 / 5.0 / 3.8.
Per sample: Chrome 215 ms, Tauri 400 ms, native 43 ms. The spline+scan stage
dominates in wasm just as natively; KPSS stays negligible.

Two webR-specific findings worth keeping:
- `read.csv(fileEncoding = "UTF-8-BOM")` relies on `iconv`, which webR lacks;
  the reader strips the BOM by hand instead.
- `library()` inside a shinylive session does not make exports visible to
  server closures; the app calls through `asNamespace()`.

Build steps:

```sh
# 1. wasm package repo (Docker; ~10–20 min under amd64 emulation)
docker run --rm --platform linux/amd64 -v "$PWD":/work -w /work ghcr.io/r-wasm/webr:main \
  Rscript -e 'pak::pak("r-wasm/rwasm"); rwasm::add_pkg("local::./core", repo_dir="wasm-repo", dependencies=FALSE); rwasm::make_vfs_library(repo_dir="wasm-repo", out_dir="wasm-repo/vfs")'
# 2. shinylive site
Rscript spike/build_site.R
Rscript -e 'httpuv::runStaticServer("spike/site", port = 8008)'
# 3. Tauri
cd dist/tauri && npx tauri dev     # then npx tauri build
```

## Tauri — resolved 2026-10-09

**`tauri://` does not work**: with `frontendDist` served by Tauri's custom
protocol the window stays blank, because shinylive's loader waits for a
service worker and WKWebView refuses to register one on a custom scheme
(asset paths are all relative, so it is not a path problem).

**Embedded localhost server works.** `dist/tauri/src-tauri/src/lib.rs` binds
`127.0.0.1:0`, serves the embedded bundle through Tauri's asset resolver via
`tiny_http` with COOP/COEP headers, and opens the main window on that URL.
Probe app (`spike/shinylive-stub`) inside the built `.app`, offline:

```
R side: R is running
crossOriginIsolated: YES
serviceWorker: YES (shinylive-sw.js)
location: http://127.0.0.1:59797/app_.../
SharedArrayBuffer: YES
R platform: wasm32-unknown-emscripten
ndiffs(kpss) works: yes
```

Server-side checks: `index.html`, `shinylive-sw.js` (`text/javascript`) and
`R.wasm` (`application/wasm`) served with correct MIME types and both
cross-origin headers. The bare shinylive runtime (no core package) is
**87.9 MB / 210 files**. Bundle targets are restricted to `app` for the
spike; the DMG step needs a signing/`create-dmg` setup later.

Build: `cd dist/tauri && npx tauri build` (≈2 min after the first compile).

## Decision (2026-10-09)

**Go: Tauri + shinylive/webR is the primary offline distribution.** Every
absolute threshold passes in both Chrome and the Tauri app; a 500-sample
batch takes 1.8 min (Chrome) / 3.5 min (WKWebView). Only the wasm/native
ratio is outside its band (5.5× / 11×), and that is the JavaScript engine,
not our code — WKWebView's wasm is about 2× slower than V8 across the board.

Carried into Phase 1 (from decision-table row 2):

1. **Chunked processing with real progress.** webR is single-threaded, so
   `withProgress()` around one long loop paints once at the start and once at
   the end. `process_set()` must take a progress callback and the app must
   process in batches that yield to the event loop (ExtendedTask natively;
   `later`/chunked observers in wasm) — the same `capabilities()` probe
   decides which.
2. **"Server mode" documentation**: for very large batches (the 786-sample
   Large.csv, ~3–6 min in-browser) the Docker/Connect native build is the
   faster path; document when to reach for it.
3. **Read path**: `read.csv` is 10 s for the 58 MB file in both runtimes
   (8.7 s natively); evaluate `data.table::fread` / `vroom` for the wide
   layout in Phase 1.
4. **Re-evaluate the ratio** after Phase 1's batching, on a Windows WebView2
   (Chromium) build, which should land near the Chrome numbers.

Bundled-R stays as the documented fallback (`dist/bundled-r/`) but is not
needed on current evidence.
