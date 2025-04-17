#' Engle–Granger Two‑Step Cointegration Test Function
#'
#' This function performs the Engle–Granger two‑step cointegration test on two time
#' series from a data frame. It first fits an OLS regression of the dependent on the
#' independent variable, then applies an Augmented Dickey‑Fuller (ADF) test to the residuals.
#'
#' @param data A data frame containing the time series data.
#' @param y_var A character string with the column name of the dependent variable.
#' @param x_var A character string with the column name of the independent variable.
#' @param lags An integer specifying the number of lags for the ADF test (default is 1).
#' @param alpha A numeric value specifying the significance level for the test (default is 0.05).
#'
#' @return A list with components:
#' \describe{
#'   \item{cointegration_model}{The fitted \code{lm} object from the OLS regression.}
#'   \item{adf_result}{The output of \code{tseries::adf.test()} applied to the residuals.}
#'   \item{conclusion}{A character string indicating whether cointegration is detected.}
#' }
#'
#' @details
#' Uses the two‑step Engle–Granger procedure:
#' \enumerate{
#'   \item Fit \code{y ~ x} by OLS, extract residuals.
#'   \item Test residuals for stationarity via \code{tseries::adf.test()}.
#' }
#' If the required package **tseries** is not installed, the function will stop and
#' prompt you to install it via:
#' \preformatted{install.packages("tseries")}
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   n <- 100
#'   x <- ts(rnorm(n))
#'   y <- ts(2 + 1.5 * x + rnorm(n))
#'   df <- data.frame(Y = y, X = x)
#'
#'   result <- egct(data   = df,
#'                  y_var  = "Y",
#'                  x_var  = "X",
#'                  lags   = 2,
#'                  alpha  = 0.05)
#'   print(result)
#' }
#'
#' @export
egct <- function(data, y_var, x_var, lags = 1, alpha = 0.05) {
  # Ensure the 'tseries' package is installed
  if (!requireNamespace("tseries", quietly = TRUE)) {
    stop("Package 'tseries' is required but not installed. Please run install.packages('tseries') and try again.")
  }

  # Extract the series
  y <- data[[y_var]]
  x <- data[[x_var]]

  # Convert to ts if needed
  if (!is.ts(y)) y <- ts(y)
  if (!is.ts(x)) x <- ts(x)

  # Step 1: OLS regression
  cointegration_model <- lm(y ~ x)
  residuals_model <- residuals(cointegration_model)
  cat("Step 1: OLS Regression Results\n")
  print(summary(cointegration_model))

  # Step 2: ADF test
  cat("\nStep 2: Augmented Dickey‑Fuller Test on Residuals\n")
  adf_result <- tseries::adf.test(residuals_model,
                                  alternative = "stationary",
                                  k = lags)
  print(adf_result)

  # Conclusion
  if (adf_result$p.value < alpha) {
    conclusion <- "Reject the null: residuals are stationary (cointegration exists)."
  } else {
    conclusion <- "Fail to reject the null: residuals are non‑stationary (no cointegration)."
  }
  cat("\nConclusion:\n", conclusion, "\n")

  invisible(list(
    cointegration_model = cointegration_model,
    adf_result          = adf_result,
    conclusion          = conclusion
  ))
}
