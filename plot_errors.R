#' Function to plot the Error Comparison (compatible with new errprof)
#'
#' @param dataIn an errprof object returned from impute_errors()/impute_errors_multi()
#' @param plotType "boxplot", "bar", or "line"
#' @param facet_by_variable logical; if TRUE and multivariate results are present, facet by Variable
#' @return A ggplot object
#' @import ggplot2
#' @export
plot_errors <- function(dataIn, plotType = c("boxplot","bar","line"), facet_by_variable = FALSE) {
  UseMethod("plot_errors")
}

#' @rdname plot_errors
#' @export
plot_errors.errprof <- function(dataIn, plotType = c("boxplot","bar","line"), facet_by_variable = FALSE) {
  plotType <- match.arg(plotType)
  if (!inherits(dataIn, "errprof")) stop("dataIn must be an 'errprof' object")

  metric <- attr(dataIn, "metric")
  errall <- attr(dataIn, "errall")
  skipped <- attr(dataIn, "skipped_methods")
  if (!is.null(skipped) && nrow(skipped) > 0) {
    message("Note: Skipped methods -> ", paste0(skipped$method, " (", skipped$reason, ")", collapse=", "))
  }

  if (is.null(errall)) stop("The 'errprof' object has no 'errall' attribute")

  # Ensure expected columns
  needed <- c("Method","Percent","Repetition","Error")
  if (!all(needed %in% names(errall))) stop("Unexpected structure in 'errall'")

  # Common aesthetics
  title_txt <- if (!is.null(metric)) paste("Distribution of", toupper(metric), "for imputed values") else "Error comparison"

  if (plotType == "boxplot") {
    df <- errall
    df$Percent <- factor(df$Percent)
    p <- ggplot(df, aes(x = Percent, y = Error, fill = Method)) +
      ggtitle(title_txt) +
      geom_boxplot() +
      labs(x = "Percent of missing observations", y = if (!is.null(metric)) toupper(metric) else "Error") +
      theme_bw()

    if (facet_by_variable && "Variable" %in% names(df)) {
      p <- p + facet_wrap(~ Variable, scales = "free_y")
    }
    return(p)
  }

  # For bar/line, average across repetitions (and variables if present)
  if ("Variable" %in% names(errall)) {
    # collapse across variables for a single overview by default
    agg <- aggregate(Error ~ Method + Percent, data = errall, FUN = mean)
  } else {
    agg <- aggregate(Error ~ Method + Percent, data = errall, FUN = mean)
  }
  agg$Percent <- factor(agg$Percent)
  ylab <- if (!is.null(metric)) toupper(metric) else "Error"

  if (plotType == "bar") {
    p <- ggplot(agg, aes(x = Percent, y = Error, fill = Method)) +
      ggtitle(title_txt) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Percent of missing observations", y = ylab) +
      theme_bw()
    return(p)
  }

  if (plotType == "line") {
    p <- ggplot(agg, aes(x = Percent, y = Error, group = Method, color = Method)) +
      ggtitle(title_txt) +
      geom_line() +
      geom_point(size = 3, alpha = 0.85) +
      labs(x = "Percent of missing observations", y = ylab) +
      theme_bw()
    return(p)
  }
}
