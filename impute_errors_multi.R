#' impute_errors_multi: multivariate benchmarking (compatible with errprof API)
#'
#' @param dataIn matrix/data.frame/tsibble (numeric columns) or a vector (falls back to impute_errors)
#' @inheritParams impute_errors
#' @return errprof-compatible data.frame with attributes:
#'         - errall (long results with Variable column; may include "STRUCT")
#'         - by_variable (mean error per Method x Percent x Variable)
#' @export
impute_errors_multi <- function(dataIn,
                                methods,
                                error = "rmse",
                                missPercent = c(5, 10, 20),
                                repetition = 5,
                                simtype = c("mcar","mar"),
                                blck = 5,
                                blckper = FALSE,
                                addl_arg = NULL,
                                parallel = c("none","future","foreach"),
                                workers = NA,
                                seed = NULL,
                                methodPath = NULL,
                                errorPath = NULL) {

  simtype   <- match.arg(simtype)
  parallel  <- match.arg(parallel)

  # Coerce input to matrix (multivariate) or route to univariate function
  if (is.numeric(dataIn) && is.vector(dataIn)) {
    # univariate → fallback to impute_errors
    if (!exists("impute_errors", mode = "function")) stop("impute_errors not found")
    return(impute_errors(dataIn, methods, error, missPercent, repetition,
                         simtype, blck, blckper, addl_arg, parallel, workers, seed, methodPath, errorPath))
  } else if (inherits(dataIn, "ts")) {
    if (!is.null(dim(dataIn)) && length(dim(dataIn)) == 2L) {
      X <- as.matrix(dataIn)
    } else {
      if (!exists("impute_errors", mode = "function")) stop("impute_errors not found")
      return(impute_errors(as.numeric(dataIn), methods, error, missPercent, repetition,
                           simtype, blck, blckper, addl_arg, parallel, workers, seed, methodPath, errorPath))
    }
  } else if (inherits(dataIn, "tbl_ts") || "tsibble" %in% class(dataIn)) {
    DF <- as.data.frame(dataIn)
    keep <- vapply(DF, is.numeric, TRUE)
    X <- as.matrix(DF[, keep, drop = FALSE])
  } else if (is.data.frame(dataIn)) {
    keep <- vapply(dataIn, is.numeric, TRUE)
    X <- as.matrix(dataIn[, keep, drop = FALSE])
  } else if (is.matrix(dataIn)) {
    X <- dataIn
  } else {
    stop("dataIn must be vector, matrix, data.frame, ts, or tsibble")
  }
  var_names <- colnames(X)
  if (is.null(var_names)) var_names <- paste0("V", seq_len(ncol(X)))

  # Optional sourcing of methods/errors
  if (!is.null(methodPath) && file.exists(methodPath)) source(methodPath, local = TRUE)
  if (!is.null(errorPath)  && file.exists(errorPath))  source(errorPath,  local = TRUE)

  # Resolve error function
  if (is.character(error)) {
    err_fun <- get(error, mode = "function")
    metric_name <- error
  } else if (is.function(error)) {
    err_fun <- error
    metric_name <- deparse(substitute(error))
  } else stop("'error' must be a function or name of a function")

  # Structural metric set (matrix → scalar)
  structural_metrics <- c("corr_preservation","cov_frobenius","struct_preservation_index","downstream_forecast_rmse")
  is_structural <- metric_name %in% structural_metrics

  # Resolve methods (via registry if available)
  resolve_from_registry <- function(nm) {
    if (exists("get_impute_method", mode = "function")) {
      info <- get_impute_method(nm)
      if (!is.null(info)) return(info)
    }
    fn <- get(nm, mode = "function")
    list(fun = fn, mode = "univariate", pkg = NULL, notes = NULL)
  }
  if (is.list(methods)) {
    method_list <- methods
    names(method_list) <- if (is.null(names(methods))) paste0("method", seq_along(methods)) else names(methods)
    method_list <- lapply(method_list, function(fn) if (is.character(fn)) get(fn, mode = "function") else fn)
    method_modes <- setNames(rep("univariate", length(method_list)), names(method_list))
  } else if (is.character(methods)) {
    infos <- lapply(methods, resolve_from_registry)
    method_list  <- lapply(infos, function(x) x$fun)
    method_modes <- setNames(vapply(infos, function(x) x$mode, character(1)), methods)
    names(method_list) <- methods
  } else stop("'methods' must be a character vector or list of functions")

  # Availability check (skip missing packages / python modules)
  skipped <- NULL
  if (is.character(methods) && exists("check_method_availability", mode = "function")) {
    av <- check_method_availability(methods, verbose = TRUE)
    # Filter to available methods
    keep <- names(method_list) %in% av$available
    method_list  <- method_list[keep]
    method_modes <- method_modes[keep]
    skipped <- av$skipped
  }

  # Storage
  long_rows <- list()

  # Loop: missingness levels
  for (p in missPercent) {
    sim <- sample_dat(x = X, b = p, r = repetition,
                      simtype = simtype, blck = blck, blckper = blckper, return_mask = TRUE)

    for (m in seq_len(repetition)) {
      Xin <- sim$X_list[[m]]   # n x k with NAs
      idx <- sim$masks[[m]]    # indices masked

      for (mn in names(method_list)) {
        fn   <- method_list[[mn]]
        mode <- method_modes[[mn]]

        if (identical(mode, "multivariate")) {
          # One call on the full matrix
          estX <- tryCatch({
            if (is.null(addl_arg)) fn(Xin) else do.call(fn, c(list(Xin), addl_arg))
          }, error = function(e) Xin)

          if (is_structural) {
            # compute metric on full matrices (ignore idx; it's about structure)
            s <- tryCatch(err_fun(X, estX), error = function(e) NA_real_)
            long_rows[[length(long_rows) + 1L]] <- data.frame(Method = mn,
                                                              Percent = p,
                                                              Repetition = m,
                                                              Variable = "STRUCT",
                                                              Error = as.numeric(s),
                                                              stringsAsFactors = FALSE)
          } else {
            # per-variable pointwise errors on masked indices
            for (j in seq_len(ncol(X))) {
              truth <- X[, j]
              est   <- estX[, j]
              e <- tryCatch(err_fun(truth[idx], est[idx]), error = function(e) NA_real_)
              long_rows[[length(long_rows) + 1L]] <- data.frame(Method = mn,
                                                                Percent = p,
                                                                Repetition = m,
                                                                Variable = var_names[j],
                                                                Error = as.numeric(e),
                                                                stringsAsFactors = FALSE)
            }
          }

        } else {
          # Univariate: loop each column; also assemble est_mat for structural metric
          est_mat <- matrix(NA_real_, nrow = nrow(X), ncol = ncol(X))
          for (j in seq_len(ncol(X))) {
            xin <- Xin[, j]
            est <- tryCatch({
              if (is.null(addl_arg)) fn(xin) else do.call(fn, c(list(xin), addl_arg))
            }, error = function(e) xin)
            est_mat[, j] <- est

            if (!is_structural) {
              truth <- X[, j]
              e <- tryCatch(err_fun(truth[idx], est[idx]), error = function(e) NA_real_)
              long_rows[[length(long_rows) + 1L]] <- data.frame(Method = mn,
                                                                Percent = p,
                                                                Repetition = m,
                                                                Variable = var_names[j],
                                                                Error = as.numeric(e),
                                                                stringsAsFactors = FALSE)
            }
          }

          if (is_structural) {
            s <- tryCatch(err_fun(X, est_mat), error = function(e) NA_real_)
            long_rows[[length(long_rows) + 1L]] <- data.frame(Method = mn,
                                                              Percent = p,
                                                              Repetition = m,
                                                              Variable = "STRUCT",
                                                              Error = as.numeric(s),
                                                              stringsAsFactors = FALSE)
          }
        }
      } # methods
    }   # repetitions
  }     # percents

  errall <- do.call(rbind, long_rows)

  # by-variable means (if STRUCT only, this will reflect that)
  by_var <- stats::aggregate(Error ~ Method + Percent + Variable, data = errall, FUN = mean)

  # collapsed across variables
  agg <- stats::aggregate(Error ~ Method + Percent, data = errall, FUN = mean)
  err_name <- metric_name
  names(agg)[names(agg) == "Error"] <- err_name
  attr(agg, "errall") <- errall
  attr(agg, "by_variable") <- by_var
  attr(agg, "metric") <- metric_name
  attr(agg, "simtype") <- simtype
  attr(agg, "repetition") <- repetition
  if (!is.null(skipped) && nrow(skipped) > 0) attr(agg, "skipped_methods") <- skipped
  class(agg) <- c("errprof", class(agg))
  return(agg)
}
