#' Multivariate Phillips–Ouliaris Cointegration Test
#'
#' This is a drop‑in replacement for `poct()` that lets you test
#' cointegration among *any* number of series in your data frame.
#'
#' @param data      A data.frame containing your time series.
#' @param vars      A character vector of column names to include (length ≥ 2).
#' @param demean    One of "none", "constant", or "trend" (default "constant").
#' @param lags      "short", "long", or an integer (default "short").
#' @param alpha     Significance level for selecting the critical value (0.01, 0.05, or 0.1; default 0.05).
#' @param type      Test statistic type: "Pz" or "Pu" (default "Pz").
#'
#' @return A list with
#' * `test_result` – the `ca.po()` object
#' * `test_stat` – the single Phillips–Ouliaris statistic
#' * `critical_values` – the full named vector of critical values
#' * `selected_critical_value` – the value matching your `alpha`
#'
#' @details
#' Combines your selected columns (≥2) into a multivariate `ts` matrix and
#' runs `urca::ca.po()`.  If **urca** is not installed, you’ll be prompted
#' to `install.packages("urca")`.
#' @export
poct <- function(data, vars,
                 demean = "constant",
                 lags   = "short",
                 alpha  = 0.05,
                 type   = "Pz") {
  if (!requireNamespace("urca", quietly = TRUE)) {
    stop("Package 'urca' is required but not installed. Please install it via install.packages('urca').")
  }
  if (length(vars) < 2) stop("Please supply at least two column names in `vars`.")

  # extract and ensure ts objects
  series_list <- lapply(vars, function(v) {
    x <- data[[v]]
    if (!is.ts(x)) x <- ts(x)
    x
  })

  # build multivariate ts matrix
  ts_matrix <- do.call(cbind, series_list)
  colnames(ts_matrix) <- vars

  # run the test
  test_result <- urca::ca.po(ts_matrix,
                             demean = demean,
                             lag    = lags,
                             type   = type)

  # unpack statistics
  test_stat <- test_result@teststat
  cval_vec  <- test_result@cval
  alpha_name <- switch(as.character(alpha),
                       "0.01" = "1pct",
                       "0.05" = "5pct",
                       "0.1"  = "10pct",
                       { warning("Unrecognized alpha; defaulting to 5pct."); "5pct" })
  crit_value <- cval_vec[alpha_name]

  # report
  cat("Phillips–Ouliaris Test on variables:", paste(vars, collapse = ", "), "\n")
  print(summary(test_result))
  cat("\nTest Statistic:", test_stat, "\n")
  cat("Critical Values:\n"); print(cval_vec)
  cat("Selected Critical Value (alpha =", alpha, "):", crit_value, "\n\n")

  invisible(list(
    test_result             = test_result,
    test_stat               = test_stat,
    critical_values         = cval_vec,
    selected_critical_value = crit_value
  ))
}
