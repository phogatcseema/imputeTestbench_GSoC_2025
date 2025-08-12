#' impute_errors: benchmark imputation methods (refactored: no eval/parse, parallel hooks)
#'
#' @param dataIn numeric vector (univariate time series)
#' @param methods character vector of function names OR list of function objects
#'                Each method must have signature: function(x, ...) returning a vector of same length
#' @param error   name or function for error metric: function(truth, estimate) -> numeric
#' @param missPercent integer vector of % missingness to simulate (e.g., c(5,10,20))
#' @param repetition integer, number of repetitions per percentage
#' @param simtype "mcar" or "mar" (block missingness)
#' @param blck integer block length when simtype = "mar"
#' @param blckper logical, if TRUE, treat blck as percentage of series length
#' @param addl_arg named list of additional args passed into method functions
#' @param parallel "none" | "future" | "foreach"
#' @param workers number of workers for parallel backends (optional)
#' @param seed optional integer for reproducibility
#' @param methodPath optional path to source() R file(s) that define extra methods
#' @param errorPath optional path to source() R file(s) that define extra error functions
#' @param plot ignored here (kept for backward-compat)
#' @return data.frame (errprof) with mean error per method and percent; attribute 'errall' with full results
#' @export
impute_errors <- function(dataIn,
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
                          errorPath = NULL,
                          plot = FALSE) {

  # ---- sanity ----
  simtype   <- match.arg(simtype)
  parallel  <- match.arg(parallel)

  # Accept common time-series classes and coerce; if multivariate, route to impute_errors_multi
  if (inherits(dataIn, "ts")) {
    if (!is.null(dim(dataIn)) && length(dim(dataIn)) == 2L) {
      # multivariate ts: route to multi wrapper
      if (!exists("impute_errors_multi", mode = "function")) stop("impute_errors_multi not found; source it before calling with multivariate data")
      return(impute_errors_multi(dataIn = as.matrix(dataIn), methods = methods, error = error,
                                 missPercent = missPercent, repetition = repetition,
                                 simtype = simtype, blck = blck, blckper = blckper, addl_arg = addl_arg,
                                 parallel = parallel, workers = workers, seed = seed, methodPath = methodPath, errorPath = errorPath))
    } else {
      dataIn <- as.numeric(dataIn)
    }
  } else if (inherits(dataIn, "zoo") || inherits(dataIn, "xts")) {
    dataIn <- tryCatch(as.numeric(zoo::coredata(dataIn)), error = function(e) as.numeric(dataIn))
  } else if (inherits(dataIn, "tbl_ts") || "tsibble" %in% class(dataIn)) {
    # coerce tsibble: keep numeric columns
    DF <- try(as.data.frame(dataIn), silent = TRUE)
    if (!inherits(DF, "try-error")) {
      keep <- vapply(DF, is.numeric, TRUE)
      DF <- DF[, keep, drop = FALSE]
      if (ncol(DF) == 1L) dataIn <- as.numeric(DF[[1]]) else {
        if (!exists("impute_errors_multi", mode = "function")) stop("impute_errors_multi not found; source it before calling with multivariate data")
        return(impute_errors_multi(dataIn = as.matrix(DF), methods = methods, error = error,
                                   missPercent = missPercent, repetition = repetition,
                                   simtype = simtype, blck = blck, blckper = blckper, addl_arg = addl_arg,
                                   parallel = parallel, workers = workers, seed = seed, methodPath = methodPath, errorPath = errorPath))
      }
    }
  } else if (is.matrix(dataIn) || is.data.frame(dataIn)) {
    # route multivariate matrix/data.frame to wrapper
    if (!exists("impute_errors_multi", mode = "function")) stop("impute_errors_multi not found; source it before calling with multivariate data")
    return(impute_errors_multi(dataIn = dataIn, methods = methods, error = error,
                               missPercent = missPercent, repetition = repetition,
                               simtype = simtype, blck = blck, blckper = blckper, addl_arg = addl_arg,
                               parallel = parallel, workers = workers, seed = seed, methodPath = methodPath, errorPath = errorPath))
  }
  if (!is.numeric(dataIn) || !is.vector(dataIn)) stop("dataIn must be a numeric vector (univariate, or use impute_errors_multi for multivariate)")

  if (!is.null(seed)) set.seed(seed)

  # source method/error libraries if provided
  if (!is.null(methodPath)) {
    if (file.exists(methodPath)) source(methodPath, local = TRUE)
  }
  if (!is.null(errorPath)) {
    if (file.exists(errorPath)) source(errorPath, local = TRUE)
  }

  # resolve methods into list of functions
  method_list <- list()
  if (is.list(methods)) {
    # assume already functions (or names mixed)
    for (i in seq_along(methods)) {
      fn <- methods[[i]]
      if (is.character(fn)) fn <- get(fn, mode = "function")
      if (!is.function(fn)) stop("Each element of 'methods' must be a function or a function name")
      method_list[[i]] <- fn
    }
    names(method_list) <- if (is.null(names(methods))) paste0("method", seq_along(methods)) else names(methods)
  } else if (is.character(methods)) {
    for (nm in methods) {
      fn <- get(nm, mode = "function")
      method_list[[nm]] <- fn
    }
  } else {
    stop("'methods' must be a character vector or a list of functions")
  }

  # resolve error function
  if (is.character(error)) {
    err_fun <- get(error, mode = "function")
    err_name <- error
  } else if (is.function(error)) {
    err_fun <- error
    err_name <- deparse(substitute(error))
  } else {
    stop("'error' must be a function or name of a function")
  }

  # addl args
  if (!is.null(addl_arg) && !is.list(addl_arg)) stop("'addl_arg' must be a named list or NULL")

  # choose backend
  do_parallel <- function(expr, n) {
    if (parallel == "none") {
      return(lapply(seq_len(n), function(i) eval(expr)))
    } else if (parallel == "future") {
      if (!requireNamespace("future.apply", quietly = TRUE)) stop("future.apply not installed")
      if (!requireNamespace("future", quietly = TRUE)) stop("future not installed")
      if (!is.na(workers)) future::plan(future::multisession, workers = workers)
      else future::plan(future::multisession)
      return(future.apply::future_lapply(seq_len(n), function(i) eval(expr)))
    } else { # foreach
      if (!requireNamespace("foreach", quietly = TRUE) || !requireNamespace("doParallel", quietly = TRUE)) {
        stop("foreach/doParallel not installed")
      }
      if (is.na(workers)) workers <- max(1L, parallel::detectCores() - 1L)
      cl <- parallel::makeCluster(workers)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      doParallel::registerDoParallel(cl)
      res <- foreach::foreach(i = seq_len(n), .packages = c()) %dopar% { eval(expr) }
      return(res)
    }
  }

  # storage
  all_rows <- list()

  # main loops
  for (p in missPercent) {
    # simulate r masks / inputs for this percent
    sim <- sample_dat(x = dataIn, b = p, r = repetition,
                      simtype = simtype, blck = blck, blckper = blckper,
                      return_mask = TRUE)

    X_list   <- sim$X_list         # list of vectors with NAs per repetition
    masks    <- sim$masks          # integer indices per repetition
    truth    <- rep(list(dataIn), repetition)

    # methods loop (each repetition uses the same method functions)
    for (m in names(method_list)) {
      fn <- method_list[[m]]

      # reps (parallelizable unit)
      compute_one <- function(rep_id) {
        xin <- X_list[[rep_id]]
        est <- tryCatch({
          if (is.null(addl_arg)) fn(xin) else do.call(fn, c(list(xin), addl_arg))
        }, error = function(e) {
          # fall back: return input (i.e., no impute) so error is measured
          xin
        })
        # compute error only at masked indices to be fair
        idx <- masks[[rep_id]]
        e <- err_fun(truth[[rep_id]][idx], est[idx])
        data.frame(Method = m, Percent = p, Repetition = rep_id, Error = as.numeric(e), stringsAsFactors = FALSE)
      }

      rows <- do_parallel(quote(compute_one(i)), repetition)
      rows <- do.call(rbind, rows)
      all_rows[[length(all_rows) + 1L]] <- rows
    }
  }

  errall <- do.call(rbind, all_rows)

  # aggregate like the original: mean error per method and percent
  agg <- stats::aggregate(Error ~ Method + Percent, data = errall, FUN = mean)
  names(agg)[names(agg) == "Error"] <- err_name
  attr(agg, "errall") <- errall
  attr(agg, "metric") <- err_name
  attr(agg, "simtype") <- simtype
  attr(agg, "repetition") <- repetition
  class(agg) <- c("errprof", class(agg))
  return(agg)
}
