#' sample_dat: simulate missingness with mask return, vector or matrix input
#'
#' @param x numeric vector, matrix, or data.frame
#' @param b integer percent missingness (0-100)
#' @param r integer repetitions
#' @param simtype "mcar" or "mar"
#' @param blck integer block length when simtype = "mar"
#' @param blckper logical, if TRUE treat blck as percentage of series length
#' @param return_mask logical, if TRUE return indices removed per repetition
#' @param seed optional integer
#' @return list with X_list (list of x with NAs for each repetition), masks (list of integer indices)
#' @export
sample_dat <- function(x, b = 10, r = 5,
                       simtype = c("mcar","mar"),
                       blck = 5, blckper = FALSE,
                       return_mask = FALSE,
                       seed = NULL) {

  simtype <- match.arg(simtype)
  if (!is.null(seed)) set.seed(seed)

  # coerce to matrix internally; keep original shape info
  if (is.vector(x)) {
    X <- matrix(as.numeric(x), ncol = 1)
  } else if (is.matrix(x)) {
    X <- x
  } else if (is.data.frame(x)) {
    X <- as.matrix(x)
  } else stop("x must be vector, matrix, or data.frame")

  n <- nrow(X)
  k <- ncol(X)

  # compute number of points to mask
  if (b <= 0) stop("b must be > 0")
  mcount <- max(1L, floor(n * (b/100)))

  # block size for MAR
  if (simtype == "mar") {
    if (blckper) blck_eff <- max(1L, floor(n * (blck/100))) else blck_eff <- max(1L, blck)
  }

  X_list  <- vector("list", r)
  masks   <- vector("list", r)

  for (i in seq_len(r)) {
    if (simtype == "mcar") {
      # random indices, avoid first/last to preserve edges
      idx_pool <- seq.int(2, n-1)
      if (length(idx_pool) < mcount) idx_pool <- seq_len(n)
      idx <- sort(sample(idx_pool, size = min(mcount, length(idx_pool)), replace = FALSE))
    } else {
      # contiguous blocks (can be multiple if mcount > block length)
      idx <- integer(0)
      remaining <- mcount
      while (remaining > 0) {
        bl <- min(remaining, blck_eff)
        start_pool <- seq.int(2, n - bl) # avoid first position
        if (length(start_pool) == 0) start_pool <- 1
        s <- sample(start_pool, 1L)
        idx_block <- seq.int(s, length.out = bl)
        idx <- sort(unique(c(idx, idx_block)))
        remaining <- mcount - length(idx)
      }
    }
    # apply same mask across columns
    Xi <- X
    Xi[idx, ] <- NA_real_
    X_list[[i]] <- if (is.vector(x)) as.vector(Xi[,1]) else Xi
    masks[[i]]  <- idx
  }

  out <- list(X_list = X_list, masks = masks, n = n, k = k, percent = b, simtype = simtype)
  class(out) <- "sample_dat_result"
  return(out)
}
