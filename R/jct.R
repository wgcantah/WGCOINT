#' Johansen Cointegration Test
#'
#' Performs the Johansen cointegration test on specified variables in a data frame.
#'
#' @param df A data frame containing your time series data.
#' @param vars A character vector of column names on which the test will be applied.
#' @param K An integer specifying the number of lags to include in the test (default: 2).
#' @param type The type of test statistic to use; either "trace" (default) or "eigen".
#' @param ecdet A string specifying the deterministic trend specification. Options are "none", "const", "trend", or "both" (default: "const").
#'
#' @return An object of class "cajo.test" with the test results. A summary is printed to the console.
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   example_df <- data.frame(
#'     var1 = cumsum(rnorm(100)),
#'     var2 = cumsum(rnorm(100)),
#'     var3 = cumsum(rnorm(100))
#'   )
#'
#'   result <- jct(example_df, vars = c("var1", "var2"), K = 2)
#' }
#'
#' @export
jct <- function(df, vars, K = 2, type = "trace", ecdet = "const") {
  # Check if the 'urca' package is installed; if not, install it automatically
  if (!requireNamespace("urca", quietly = TRUE)) {
    install.packages("urca")
  }

  # Verify that all indicated variables exist in the data frame
  if (!all(vars %in% names(df))) {
    stop("Some of the specified variables are not present in the data frame.")
  }

  # Subset the data frame to only include the required columns
  sub_df <- df[, vars, drop = FALSE]

  # Ensure that all selected columns are numeric
  if (!all(sapply(sub_df, is.numeric))) {
    stop("All selected variables must be numeric for the Johansen test.")
  }

  # Run the Johansen cointegration test using the ca.jo function from urca
  test_result <- urca::ca.jo(sub_df, type = type, ecdet = ecdet, K = K)

  # Display a summary of the test results
  print(summary(test_result))

  # Return the test result invisibly so that it can be stored for further analysis if required
  invisible(test_result)
}
