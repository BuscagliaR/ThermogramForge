# Build the shinylive site for the Phase-0 spike.
# Run from the repo root after wasm-repo/ has been produced (see spike/README.md):
#   Rscript spike/build_site.R
# Then serve:  Rscript -e 'httpuv::runStaticServer("spike/site", port = 8008)'

app_dir <- "spike/shinylive-app"
site_dir <- "spike/site"

stopifnot(dir.exists("wasm-repo"))
if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("install.packages('shinylive') first")
}

# Fixture and the wasm package repo travel with the app.
dir.create(file.path(app_dir, "data"), showWarnings = FALSE)
file.copy("core/tests/testthat/fixtures/raw/Small.csv",
          file.path(app_dir, "data", "Small.csv"), overwrite = TRUE)
# Anything inside app_dir is inlined into app.json (shinylive's virtual FS),
# which is not reachable over HTTP. webr::install()/mount() fetch by URL, so
# the wasm repo must sit beside the exported site, not inside the app.
unlink(file.path(app_dir, "wasm-repo"), recursive = TRUE)

unlink(site_dir, recursive = TRUE)
shinylive::export(app_dir, site_dir, wasm_packages = TRUE, package_cache = TRUE)
# Add thermogramforge.core to shinylive's own package bundle so it is
# installed before the app starts, exactly like the CRAN packages. The
# bundle is shinylive/webr/packages/<name>/<name>_<ver>.tgz plus an entry in
# packages/metadata.rds (shape copied from a shinylive-generated entry).
tgz <- list.files("wasm-repo/bin/emscripten/contrib", pattern = "^thermogramforge\\.core_.*\\.tgz$",
                  recursive = TRUE, full.names = TRUE)
stopifnot(length(tgz) == 1L)
ver <- sub("^thermogramforge\\.core_(.*)\\.tgz$", "\\1", basename(tgz))
pkg_dir <- file.path(site_dir, "shinylive", "webr", "packages", "thermogramforge.core")
dir.create(pkg_dir, showWarnings = FALSE)
file.copy(tgz, file.path(pkg_dir, basename(tgz)), overwrite = TRUE)
meta_path <- file.path(site_dir, "shinylive", "webr", "packages", "metadata.rds")
meta <- readRDS(meta_path)
meta[["thermogramforge.core"]] <- list(
  name = c(thermogramforge.core = "thermogramforge.core"),
  version = ver,
  cached = TRUE,
  assets = list(list(filename = basename(tgz),
                     url = paste0("./wasm-repo/bin/emscripten/contrib/4.6/", basename(tgz)),
                     version = ver)),
  type = "package",
  ref = paste0("thermogramforge.core@", ver),
  path = paste0("packages/thermogramforge.core/", basename(tgz))
)
saveRDS(meta, meta_path)
cat(sprintf("bundled thermogramforge.core %s into shinylive packages (%d packages)\n",
            ver, length(meta)))

size_mb <- sum(file.info(list.files(site_dir, recursive = TRUE,
                                    full.names = TRUE))$size) / 1024^2
cat(sprintf("site built at %s (%.1f MB)\n", site_dir, size_mb))
