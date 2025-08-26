# Simple registry for imputation methods, and their capabilities
# Each entry: list(fun = function, mode = "univariate"|"multivariate", pkg = NULL, notes = NULL)

# Preserve registry across re-sourcing
if (!exists(".imptb_registry", inherits = FALSE) || !is.environment(.imptb_registry)) {
  .imptb_registry <- new.env(parent = emptyenv())
}

#' Register an imputation method
#' @param name unique method name (character)
#' @param fun function object
#' @param mode "univariate" or "multivariate"
#' @param pkg optional package name the method relies on
#' @param notes optional notes string
#' @export
register_impute_method <- function(name, fun, mode = c("univariate","multivariate"), pkg = NULL, notes = NULL) {
  mode <- match.arg(mode)
  if (!is.character(name) || length(name) != 1L) stop("'name' must be a single character string")
  if (!is.function(fun)) stop("'fun' must be a function")
  .imptb_registry[[name]] <- list(fun = fun, mode = mode, pkg = pkg, notes = notes)
  invisible(TRUE)
}

#' Retrieve registered method info
#' @param name method name
#' @return list with fun/mode/pkg/notes or NULL if not found
#' @export
get_impute_method <- function(name) {
  if (!exists(name, envir = .imptb_registry, inherits = FALSE)) return(NULL)
  get(name, envir = .imptb_registry, inherits = FALSE)
}

#' List registered methods
#' @export
list_impute_methods <- function() {
  ls(envir = .imptb_registry, all.names = FALSE)
}

#' Clear registry (use with caution)
#' @export
clear_impute_registry <- function() {
  rm(list = ls(.imptb_registry), envir = .imptb_registry)
  invisible(TRUE)
}


#' Check method availability and (optionally) skip missing ones
#' @param methods character vector of method names
#' @param verbose logical; if TRUE, message about skipped methods
#' @return list(available=character(), skipped=data.frame(method, reason))
#' @export
check_method_availability <- function(methods, verbose = TRUE) {
  avail <- character(0)
  skipped <- data.frame(method = character(0), reason = character(0), stringsAsFactors = FALSE)

  for (nm in methods) {
    info <- get_impute_method(nm)
    if (is.null(info)) {
      # not in registry; assume a user-supplied function by name exists in env
      # keep it, but warn later if it's really missing at call-time
      avail <- c(avail, nm)
      next
    }
    pkg <- info$pkg
    ok  <- TRUE
    reason <- NA_character_
    if (!is.null(pkg)) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        ok <- FALSE; reason <- paste0("Missing package '", pkg, "'")
      } else if (identical(pkg, "reticulate") && !is.null(info$notes)) {
        # optional: check python module if notes specifies it
        mod <- info$notes
        if (isTRUE(requireNamespace("reticulate", quietly = TRUE))) {
          has <- tryCatch(reticulate::py_module_available(mod), error = function(e) FALSE)
          if (!isTRUE(has)) { ok <- FALSE; reason <- paste0("Python module '", mod, "' not available") }
        }
      }
    }
    if (ok) avail <- c(avail, nm) else skipped <- rbind(skipped, data.frame(method = nm, reason = reason, stringsAsFactors = FALSE))
  }

  if (verbose && nrow(skipped) > 0) {
    msg <- paste0("Skipping methods: ", paste0(skipped$method, " (", skipped$reason, ")", collapse = ", "))
    message(msg)
  }
  list(available = unique(avail), skipped = skipped)
}

