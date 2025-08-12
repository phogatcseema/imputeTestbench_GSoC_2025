# Demo: using the method registry and multivariate-capable methods

# source("methods_registry.R")
# source("methods_builtin.R")
# source("sample_dat.R")
# source("impute_errors_multi.R")
# source("impute_errors.R")  # for ts auto-routing

# quick data
set.seed(42)
n <- 300
u <- sin(seq(0, 6*pi, length.out = n)) + rnorm(n, sd = 0.1)
X <- cbind(A = u, B = u + rnorm(n, sd = 0.1))

# Choose methods: some univariate, some multivariate
meths <- c("mean_impute", "kalman_structTS", "vim_knn_multi")

# Run benchmark (will call vim_knn_multi once per repetition on the matrix; others per-column)
res <- impute_errors_multi(
  dataIn = X,
  methods = meths,
  error   = "rmse",
  missPercent = c(10, 30),
  repetition  = 2,
  simtype     = "mcar"
)

print(res)
head(attr(res, "by_variable"))
head(attr(res, "errall"))
