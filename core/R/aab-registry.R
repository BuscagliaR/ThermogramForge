# ==============================================================================
# Method registry
# ==============================================================================
# Methods register themselves with a parameter spec. The spec is what the
# Shiny layer renders, so adding a method requires no UI code.
#
# A param spec entry:
#   list(name = "w", type = "integer", default = 90L, min = 30L, max = 200L,
#        label = "Window size (points)")
# type is one of "integer", "numeric", "choice" (then `choices` is required),
# "logical".

.registry <- new.env(parent = emptyenv())
.registry$endpoint <- list()
.registry$signal <- list()

#' Register a method
#'
#' @param id Registry key, e.g. `"rolling_sd"`.
#' @param kind `"endpoint"` or `"signal"`.
#' @param constructor Function returning a method object. Its formals must
#'   match the `name`s in `params`.
#' @param params List of parameter spec entries (see file header).
#' @param label Human-readable name.
#' @param reference Logical; frozen implementation kept for reproducibility.
#' @param default Logical; the method the app selects initially.
#' @export
register_method <- function(id, kind = c("endpoint", "signal"), constructor,
                            params = list(), label = id, reference = FALSE,
                            default = FALSE) {
  kind <- match.arg(kind)
  stopifnot(is.character(id), length(id) == 1L, is.function(constructor))
  for (p in params) {
    stopifnot(is.list(p), all(c("name", "type", "default") %in% names(p)))
    if (identical(p$type, "choice")) stopifnot(!is.null(p$choices))
  }
  entry <- list(id = id, kind = kind, constructor = constructor,
                params = params, label = label, reference = reference,
                default = default)
  .registry[[kind]][[id]] <- entry
  invisible(entry)
}

#' List registered methods
#'
#' @param kind `"endpoint"` or `"signal"`.
#' @return Named list of registry entries.
#' @export
list_methods <- function(kind = c("endpoint", "signal")) {
  kind <- match.arg(kind)
  .registry[[kind]]
}

#' Fetch one registry entry
#' @inheritParams register_method
#' @export
get_method <- function(id, kind = c("endpoint", "signal")) {
  kind <- match.arg(kind)
  entry <- .registry[[kind]][[id]]
  if (is.null(entry)) {
    stop_input(sprintf("no %s method registered as '%s'", kind, id))
  }
  entry
}

#' Build a method object from its registry id and parameter values
#'
#' Unspecified parameters take their spec defaults.
#' @inheritParams register_method
#' @param ... Parameter values by name.
#' @export
make_method <- function(id, kind = c("endpoint", "signal"), ...) {
  kind <- match.arg(kind)
  entry <- get_method(id, kind)
  defaults <- lapply(entry$params, `[[`, "default")
  names(defaults) <- vapply(entry$params, `[[`, "", "name")
  args <- utils::modifyList(defaults, list(...))
  do.call(entry$constructor, args)
}

#' The default method id for a kind
#' @inheritParams list_methods
#' @export
default_method_id <- function(kind = c("endpoint", "signal")) {
  kind <- match.arg(kind)
  entries <- .registry[[kind]]
  hit <- Filter(function(e) isTRUE(e$default), entries)
  if (length(hit) == 0L) return(NULL)
  hit[[1]]$id
}
