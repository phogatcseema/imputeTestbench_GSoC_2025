# Built-in imputation methods and registrations

# ---- Univariate wrappers ----
mean_impute <- function(x, ...) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}

locf_impute <- function(x, ...) {
  if (!requireNamespace("zoo", quietly = TRUE)) stop("Please install 'zoo' for locf_impute")
  zoo::na.locf(x, na.rm = FALSE)
}

interp_linear <- function(x, ...) {
  if (!requireNamespace("imputeTS", quietly = TRUE)) stop("Please install 'imputeTS' for interp_linear")
  imputeTS::na_interpolation(x, option = "linear")
}

interp_spline <- function(x, ...) {
  if (!requireNamespace("imputeTS", quietly = TRUE)) stop("Please install 'imputeTS' for interp_spline")
  imputeTS::na_interpolation(x, option = "spline")
}

kalman_structTS <- function(x, ...) {
  if (!requireNamespace("imputeTS", quietly = TRUE)) stop("Please install 'imputeTS' for kalman_structTS")
  imputeTS::na_kalman(x, model = "StructTS", smooth = TRUE)
}

# ---- Multivariate wrappers ----
missForest_impute_multi <- function(X, ...) {
  if (!requireNamespace("missForest", quietly = TRUE)) stop("Please install 'missForest' for missForest_impute_multi")
  out <- missForest::missForest(as.data.frame(X), ...)
  as.matrix(out$ximp)
}

mice_pmm_multi <- function(X, m = 1, maxit = 5, method = "pmm", seed = NULL, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) stop("Please install 'mice' for mice_pmm_multi")
  if (!is.null(seed)) set.seed(seed)
  imp <- mice::mice(as.data.frame(X), m = m, maxit = maxit, method = method, printFlag = FALSE, ...)
  comp <- mice::complete(imp, action = 1)
  as.matrix(comp)
}

vim_knn_multi <- function(X, k = 5, ...) {
  if (!requireNamespace("VIM", quietly = TRUE)) stop("Please install 'VIM' for vim_knn_multi")
  df <- as.data.frame(X)
  out <- VIM::kNN(df, k = k, imp_var = FALSE, ...)
  as.matrix(out[, colnames(df), drop = FALSE])
}

# ---- Optional Python (reticulate) ----
py_sklearn_knn_multi <- function(X, n_neighbors = 5, weights = "uniform", ...) {
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("Please install 'reticulate' for py_sklearn_knn_multi")
  reticulate::py_config() # trigger python discovery
  sklearn <- try(reticulate::import("sklearn.impute", delay_load = TRUE), silent = TRUE)
  if (inherits(sklearn, "try-error")) stop("Python sklearn not available in current environment")
  imputer <- sklearn$KNNImputer(n_neighbors = as.integer(n_neighbors), weights = weights)
  res <- imputer$fit_transform(X)
  # convert from python numpy array to R matrix
  if (reticulate::py_has_attr(res, "astype")) {
    res <- res$astype("float64")
  }
  as.matrix(res)
}

# ---- Register built-ins if registry exists ----
if (exists("register_impute_method", mode = "function")) {
  register_impute_method("mean_impute", mean_impute, mode = "univariate")
  register_impute_method("locf_impute",  locf_impute,  mode = "univariate", pkg = "zoo")
  register_impute_method("interp_linear", interp_linear, mode = "univariate", pkg = "imputeTS")
  register_impute_method("interp_spline", interp_spline, mode = "univariate", pkg = "imputeTS")
  register_impute_method("kalman_structTS", kalman_structTS, mode = "univariate", pkg = "imputeTS")

  register_impute_method("missForest_impute_multi", missForest_impute_multi, mode = "multivariate", pkg = "missForest")
  register_impute_method("mice_pmm_multi",        mice_pmm_multi,        mode = "multivariate", pkg = "mice")
  register_impute_method("vim_knn_multi",         vim_knn_multi,         mode = "multivariate", pkg = "VIM")
  register_impute_method("py_sklearn_knn_multi",  py_sklearn_knn_multi,  mode = "multivariate", pkg = "reticulate", notes = "sklearn.impute")
}


# Idempotent registration helper
register_builtin_methods <- function() {
  if (!exists("register_impute_method", mode = "function")) stop("registry not loaded; source('methods_registry.R') first")
  register_impute_method("mean_impute", mean_impute, mode = "univariate")
  register_impute_method("locf_impute",  locf_impute,  mode = "univariate", pkg = "zoo")
  register_impute_method("interp_linear", interp_linear, mode = "univariate", pkg = "imputeTS")
  register_impute_method("interp_spline", interp_spline, mode = "univariate", pkg = "imputeTS")
  register_impute_method("kalman_structTS", kalman_structTS, mode = "univariate", pkg = "imputeTS")

  register_impute_method("missForest_impute_multi", missForest_impute_multi, mode = "multivariate", pkg = "missForest")
  register_impute_method("mice_pmm_multi",        mice_pmm_multi,        mode = "multivariate", pkg = "mice")
  register_impute_method("vim_knn_multi",         vim_knn_multi,         mode = "multivariate", pkg = "VIM")
  register_impute_method("py_sklearn_knn_multi",  py_sklearn_knn_multi,  mode = "multivariate", pkg = "reticulate", notes = "sklearn.impute")
  invisible(TRUE)
}

# Auto-register on source if registry is present
if (exists("register_impute_method", mode = "function")) {
  register_builtin_methods()
}
