# Error / quality metrics for imputation benchmarks

# --- pointwise metrics (univariate-friendly) ---
rmse <- function(truth, estimate) {
  sqrt(mean((truth - estimate)^2, na.rm = TRUE))
}

mae <- function(truth, estimate) {
  mean(abs(truth - estimate), na.rm = TRUE)
}

mape <- function(truth, estimate) {
  # guard against zeros in truth
  denom <- ifelse(abs(truth) < .Machine$double.eps, NA_real_, abs(truth))
  mean(abs((truth - estimate) / denom), na.rm = TRUE)
}

# --- structural (multivariate) metrics ---
# All return an "error" to be minimized; lower is better.

# mean absolute difference of correlation coefficients (upper triangle)
corr_preservation <- function(X_true, X_hat) {
  C0 <- stats::cor(X_true, use = "pairwise.complete.obs")
  C1 <- stats::cor(X_hat,  use = "pairwise.complete.obs")
  ut <- upper.tri(C0, diag = FALSE)
  mean(abs(C0[ut] - C1[ut]), na.rm = TRUE)
}

# Frobenius norm of covariance difference
cov_frobenius <- function(X_true, X_hat) {
  S0 <- stats::cov(X_true, use = "pairwise.complete.obs")
  S1 <- stats::cov(X_hat,  use = "pairwise.complete.obs")
  E  <- S1 - S0
  sqrt(sum(E*E, na.rm = TRUE))
}

# Combined normalized structural preservation index (0..1-ish; lower is better)
struct_preservation_index <- function(X_true, X_hat) {
  ce <- corr_preservation(X_true, X_hat) # in [0,2]
  se <- cov_frobenius(X_true, X_hat)     # scale-dependent
  # normalize
  ce_n <- ce / 2
  S0 <- stats::cov(X_true, use = "pairwise.complete.obs")
  denom <- sqrt(sum(S0*S0, na.rm = TRUE))
  se_n <- if (is.finite(denom) && denom > 0) se / denom else NA_real_
  mean(c(ce_n, se_n), na.rm = TRUE)
}

# Optional downstream metric (requires forecast)
downstream_forecast_rmse <- function(X_true, X_hat, h = 6) {
  if (!requireNamespace("forecast", quietly = TRUE)) stop("Please install 'forecast' for downstream_forecast_rmse")
  k <- ncol(X_true)
  errs <- numeric(k)
  for (j in seq_len(k)) {
    x  <- as.numeric(X_hat[, j])
    y  <- as.numeric(X_true[, j])
    n  <- length(x)
    tr <- seq_len(max(1, n - h))
    ts_tr <- stats::ts(x[tr])
    fit <- suppressWarnings(forecast::auto.arima(ts_tr))
    fc  <- suppressWarnings(forecast::forecast(fit, h = h))
    yh  <- as.numeric(fc$mean)
    yt  <- y[(max(1, n - h) + 1):n]
    errs[j] <- sqrt(mean((yt - yh)^2, na.rm = TRUE))
  }
  mean(errs, na.rm = TRUE)
}
