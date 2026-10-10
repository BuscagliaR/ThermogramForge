# Write the first 500 sample pairs of data-raw/Large.csv (786 samples, wide
# T1a/1a pair layout) to core/tests/testthat/fixtures/large/Large500.csv.
# The output is gitignored; it exists only for the Phase-0 timing gate.
# Run from the repo root: Rscript core/data-raw/make_large500.R

src <- "data-raw/Large.csv"
dst <- "core/tests/testthat/fixtures/large/Large500.csv"
stopifnot(file.exists(src))

header <- readLines(src, n = 1L, encoding = "UTF-8")
header <- sub("^﻿", "", header)
cols <- strsplit(header, ",", fixed = TRUE)[[1]]
n_pairs <- length(cols) %/% 2
keep <- seq_len(min(500L, n_pairs) * 2L)

x <- utils::read.csv(src, check.names = FALSE, fileEncoding = "UTF-8-BOM")
x <- x[, keep]
dir.create(dirname(dst), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(x, dst, row.names = FALSE)
cat(sprintf("Wrote %s: %d rows x %d cols (%d samples)\n",
            dst, nrow(x), ncol(x), ncol(x) %/% 2))
