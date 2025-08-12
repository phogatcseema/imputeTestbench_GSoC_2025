# Demo script to check refactored functions
# Usage: in R, setwd to this folder and run: source("demo_gsoc_check.R")

# ---- load helpers ----
# source("sample_dat.R")
# source("impute_errors.R")
# source("impute_errors_multi.R")

# simple imputation methods ----
mean_impute <- function(x, ...) { x[is.na(x)] <- mean(x, na.rm = TRUE); x }
locf_impute <- function(x, ...) {
  if (!requireNamespace("zoo", quietly = TRUE)) stop("Please install 'zoo' for locf_impute")
  zoo::na.locf(x, na.rm = FALSE)
}

# simple error metric if not already defined in your package ----
rmse <- function(truth, estimate) sqrt(mean((truth - estimate)^2, na.rm = TRUE))

set.seed(123)
# univariate toy series
ts_u <- sin(seq(0, 8*pi, length.out = 500)) + rnorm(500, sd = 0.1)

cat("\n--- Univariate benchmark ---\n")
res_u <- impute_errors(
  dataIn = ts_u,
  methods = c("mean_impute", "locf_impute"),
  error = "rmse",
  missPercent = c(10, 30),
  repetition = 3,
  simtype = "mar",
  blck = 10
)
print(res_u)
cat("\nerrall head:\n")
print(head(attr(res_u, "errall")))

# multivariate toy matrix (two correlated series)
ts_v1 <- ts_u
ts_v2 <- ts_u + rnorm(500, sd = 0.05)
X <- cbind(A = ts_v1, B = ts_v2)

cat("\n--- Multivariate benchmark ---\n")
res_m <- impute_errors_multi(
  dataIn = X,
  methods = c("mean_impute", "locf_impute"),
  error = "rmse",
  missPercent = c(10, 30),
  repetition = 3,
  simtype = "mcar"
)
print(res_m)
cat("\nby_variable head:\n")
print(head(attr(res_m, "by_variable")))


#####

#source("plot_errors.txt")   # if your plotting file is named like this
plot_errors(res_u, plotType = "line")   # or "bar"/"line" depending on your API


###########

library(ggplot2)
errall_m <- attr(res_m, "errall")
ggplot(errall_m, aes(x = factor(Percent), y = Error, fill = Method)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(x = "% Missing", y = "RMSE", title = "Multivariate imputation benchmark")


############

# install.packages(c("future.apply","future","doParallel","foreach"))


###########

res_u_par <- impute_errors(
  dataIn = ts_u,
  methods = c("mean_impute", "locf_impute"),
  error = "rmse",
  missPercent = c(10, 30),
  repetition = 8,
  simtype = "mcar",
  parallel = "future",   # or "foreach"
  workers  = 4
)
print(res_u_par)


###########

# simple methods + metric for the test
mean_impute <- function(x, ...) { x[is.na(x)] <- mean(x, na.rm = TRUE); x }
rmse <- function(truth, estimate) sqrt(mean((truth - estimate)^2, na.rm = TRUE))

res_nottem <- impute_errors(
  dataIn = nottem,                      # ts object now accepted
  methods = c("mean_impute"),
  error   = "rmse",
  missPercent = c(10, 20),
  repetition  = 3,
  simtype = "mcar"
)
print(res_nottem)
head(attr(res_nottem, "errall"))


##############

# make a 2-var multivariate ts
m <- cbind(nottem, nottem + rnorm(length(nottem), sd = 1))
mts <- ts(m, start = start(nottem), frequency = frequency(nottem))

locf_impute <- function(x, ...) { if(!requireNamespace("zoo", quietly=TRUE)) install.packages("zoo"); zoo::na.locf(x, na.rm = FALSE) }

res_multi <- impute_errors(           # note: calling impute_errors() is fine
  dataIn = mts,                       # multivariate ts → routed to impute_errors_multi()
  methods = c("mean_impute", "locf_impute"),
  error   = "rmse",
  missPercent = c(10, 20),
  repetition  = 3,
  simtype = "mar",
  blck = 6
)
print(res_multi)
head(attr(res_multi, "by_variable"))  # per-variable means


#############


# source("sample_dat.R")
# source("impute_errors_multi.R")
# source("impute_errors.R")      # patched to accept ts/zoo/xts

# simple imputation methods
mean_impute <- function(x, ...) { x[is.na(x)] <- mean(x, na.rm = TRUE); x }
locf_impute <- function(x, ...) { if(!requireNamespace("zoo", quietly=TRUE)) install.packages("zoo"); zoo::na.locf(x, na.rm = FALSE) }
rmse <- function(truth, estimate) sqrt(mean((truth - estimate)^2, na.rm = TRUE))

aa <- impute_errors(
  dataIn = nottem,                         # 'ts' now accepted
  methods = c("mean_impute", "locf_impute"),
  error   = "rmse",
  missPercent = c(10, 20),
  repetition  = 3,
  simtype     = "mcar"
)


#source("plot_errors.R")

# default boxplot of full error distributions
p1 <- plot_errors(aa, plotType = "boxplot")
print(p1)

# bar of averages
p2 <- plot_errors(aa, plotType = "bar")
print(p2)

# line of averages
p3 <- plot_errors(aa, plotType = "line")
print(p3)

############

m <- cbind(nottem, nottem + rnorm(length(nottem), sd = 1))
mts <- ts(m, start = start(nottem), frequency = frequency(nottem))

bb <- impute_errors(
  dataIn = mts,   # multivariate ts → routed to impute_errors_multi()
  methods = c("mean_impute", "locf_impute"),
  error   = "rmse",
  missPercent = c(10, 20),
  repetition  = 3,
  simtype     = "mar",
  blck        = 6
)

# Boxplot faceted by variable
plot_errors(bb, plotType = "boxplot", facet_by_variable = TRUE)


#################

set.seed(42)
n <- 300
u <- sin(seq(0, 6*pi, length.out = n)) + rnorm(n, sd = 0.1)
X <- cbind(A = u, B = u + rnorm(n, sd = 0.1))

list_impute_methods()
# Expect: mean_impute, locf_impute, interp_linear, interp_spline, kalman_structTS,
#         missForest_impute_multi, mice_pmm_multi, vim_knn_multi, py_sklearn_knn_multi


# Choose methods (will use multivariate jointly where available)
meths <- c("mean_impute", "kalman_structTS", "vim_knn_multi", "mice_pmm_multi")  # VIM::kNN is multivariate

rmse <- function(truth, estimate) sqrt(mean((truth - estimate)^2, na.rm = TRUE))

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


p <- plot_errors(res, plotType = "boxplot", facet_by_variable = TRUE)
print(p)


###############

# source("methods_registry.R")
# source("methods_builtin.R")
# source("error_functions.R")
# source("sample_dat.R")
# source("impute_errors_multi.R")
# source("impute_errors.R")
# source("plot_errors.R")

set.seed(123)
n <- 300
u <- sin(seq(0, 6*pi, length.out = n)) + rnorm(n, 0, 0.15)
X <- cbind(A = u, B = 0.8*u + rnorm(n, 0, 0.15), C = 0.2*u + rnorm(n, 0, 0.15))

meths <- c("mean_impute","kalman_structTS","vim_knn_multi")
rmse  <- function(truth, estimate) sqrt(mean((truth - estimate)^2, na.rm = TRUE))

# Pointwise (per-variable)
res_rmse <- impute_errors_multi(X, methods = meths, error = "rmse",
                                missPercent = c(10, 30), repetition = 3, simtype = "mcar")
plot_errors(res_rmse, "boxplot", facet_by_variable = TRUE)

# Structural (joint metric on full matrices)
res_corr <- impute_errors_multi(X, methods = meths, error = "corr_preservation",
                                missPercent = c(10, 30), repetition = 3, simtype = "mar", blck = 8)
print(head(attr(res_corr, "errall")))      # you'll see Variable = "STRUCT"
plot_errors(res_corr, "boxplot", facet_by_variable = TRUE)
