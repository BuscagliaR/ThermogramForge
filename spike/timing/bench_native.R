# Native-R side of the Phase-0 timing gate.
# Run from the repo root:  Rscript spike/timing/bench_native.R
# Writes spike/timing/native_results.csv and prints a summary.

suppressMessages(devtools::load_all("core", quiet = TRUE))

small <- read_thermogram_csv("core/tests/testthat/fixtures/raw/Small.csv")
large_path <- "core/tests/testthat/fixtures/large/Large500.csv"
if (!file.exists(large_path)) {
  stop("Large500.csv missing; run Rscript core/data-raw/make_large500.R first")
}
t_read <- system.time(large <- read_thermogram_csv(large_path))[["elapsed"]]
cat(sprintf("read Large500.csv: %.2fs (%d samples)\n", t_read, n_samples(large)))

runs <- list(
  small_default = bench_pipeline(small, endpoint_method = ep_rolling_sd(),
                                 signal_method = sig_kpss()),
  small_v1      = bench_pipeline(small, endpoint_method = ep_rolling_sd_v1(),
                                 signal_method = sig_auto_arima_v1()),
  large500_default = bench_pipeline(large, endpoint_method = ep_rolling_sd(),
                                    signal_method = sig_kpss())
)

for (nm in names(runs)) cat(sprintf("%-18s %s\n", nm, format_bench(runs[[nm]])))

out <- do.call(rbind, lapply(names(runs), function(nm) {
  b <- runs[[nm]]
  data.frame(run = nm, n = b$n, stage = names(b$totals), seconds = unname(b$totals),
             endpoint_method = b$session$endpoint_method,
             signal_method = b$session$signal_method,
             platform = b$session$platform, R = b$session$R)
}))
out <- rbind(out, data.frame(run = "large500_default", n = n_samples(large),
                             stage = "read_csv", seconds = t_read,
                             endpoint_method = "", signal_method = "",
                             platform = R.version$platform, R = R.version.string))
write.csv(out, "spike/timing/native_results.csv", row.names = FALSE)
cat("wrote spike/timing/native_results.csv\n")
