# ==============================================================================
# ThermogramSet (Phase-0 minimal) and CSV reader
# ==============================================================================
# Just enough to feed the spike: a named list of per-sample Temperature/dCp
# data.frames plus provenance. The full typed set (grid, signal matrix,
# endpoints table, flags) arrives in Phase 1.

#' A set of raw thermograms
#'
#' @param samples Named list of data.frames with columns `Temperature`, `dCp`.
#' @param source Where the data came from (file path or description).
#' @param format One of `"wide"`, `"long"`, `"single"`.
#' @export
ThermogramSet <- S7::new_class(
  "ThermogramSet",
  properties = list(
    samples = S7::class_list,
    source = S7::class_character,
    format = S7::class_character
  ),
  validator = function(self) {
    if (length(self@samples) == 0L) return("@samples must not be empty")
    if (is.null(names(self@samples)) || any(names(self@samples) == "")) {
      return("@samples must be a named list")
    }
    for (nm in names(self@samples)) {
      s <- self@samples[[nm]]
      if (!is.data.frame(s) || !all(c("Temperature", "dCp") %in% names(s))) {
        return(sprintf("sample '%s' must be a data.frame with Temperature and dCp", nm))
      }
    }
    NULL
  }
)

#' Read thermograms from a CSV file
#'
#' Detects the three V1 layouts:
#' * wide: pairs of columns `T<id>` / `<id>` (e.g. `T1a`, `1a`)
#' * long: `Sample_ID`, `Temperature`, `dCp`
#' * single: `Temperature`, `dCp`
#'
#' A UTF-8 byte-order mark is tolerated. In the wide layout, rows where either
#' column of a pair is `NA` are dropped for that sample (samples may have
#' different lengths), matching V1's `complete.cases()` handling.
#'
#' @param path Path to the CSV file.
#' @return A [ThermogramSet].
#' @export
read_thermogram_csv <- function(path) {
  if (!file.exists(path)) stop_input(sprintf("file not found: %s", path))
  # No fileEncoding = "UTF-8-BOM": that goes through iconv, which webR lacks.
  # Strip a byte-order mark from the first column name by hand instead.
  x <- utils::read.csv(path, check.names = FALSE, encoding = "UTF-8",
                       stringsAsFactors = FALSE)
  names(x)[1L] <- sub("^\ufeff", "", names(x)[1L])
  cols <- names(x)

  if (all(c("Sample_ID", "Temperature", "dCp") %in% cols)) {
    ids <- as.character(unique(x$Sample_ID))
    samples <- lapply(ids, function(id) {
      s <- x[x$Sample_ID == id, c("Temperature", "dCp")]
      rownames(s) <- NULL
      s
    })
    names(samples) <- ids
    return(ThermogramSet(samples = samples, source = path, format = "long"))
  }

  if (identical(sort(cols), sort(c("Temperature", "dCp")))) {
    s <- x[, c("Temperature", "dCp")]
    return(ThermogramSet(samples = list(sample = s), source = path,
                         format = "single"))
  }

  temp_cols <- cols[startsWith(cols, "T")]
  ids <- sub("^T", "", temp_cols)
  ids <- ids[ids %in% cols]
  if (length(ids) == 0L) {
    stop_input("could not detect a thermogram layout (wide, long or single)")
  }
  samples <- list()
  for (id in ids) {
    s <- data.frame(Temperature = as.numeric(x[[paste0("T", id)]]),
                    dCp = as.numeric(x[[id]]))
    s <- s[stats::complete.cases(s), , drop = FALSE]
    rownames(s) <- NULL
    if (nrow(s) > 0L) samples[[id]] <- s
  }
  ThermogramSet(samples = samples, source = path, format = "wide")
}

#' Number of samples in a set
#' @param x A [ThermogramSet].
#' @export
n_samples <- function(x) length(x@samples)
