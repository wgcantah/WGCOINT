#' Gregory–Hansen Cointegration Test with Structural Break
#'
#' @description
#' Performs the Gregory–Hansen (1996) cointegration test between a dependent series and one or more regressors,
#' allowing for a one-time level, level-and-trend, or regime shift.
#'
#' @param data A data.frame containing numeric columns.
#' @param y Name of the dependent variable (length-1 character).
#' @param x Character vector of one or more independent variables.
#' @param shift One of "level", "level-trend" or "regime" (default "level").
#' @param trim Fraction to trim at each end (default 0.15).
#' @param max.lag Maximum lag order for the ADF test on residuals (default 10).
#' @param criterion Information criterion for lag selection: "aic", "bic", or "hq".
#'
#' @return An object of class "cointGH" containing Za, Zt, and ADF statistics with critical values.
#'
#' @examples
#' set.seed(123)
#' n <- 200
#' x1 <- cumsum(rnorm(n))
#' x2 <- cumsum(rnorm(n))
#' x3 <- cumsum(rnorm(n))
#' tau <- 100
#' y <- cumsum(rnorm(n)) +
#'      ifelse(seq_len(n) > tau, 2, 0) * x1 +
#'      0.7 * x2 -
#'      0.3 * x3
#' df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
#' res <- ghct(df, y = "y", x = c("x1", "x2", "x3"))
#' print(res)
#'
#' @export
ghct <- function(data,
                                y,
                                x,
                                shift     = c("level", "level-trend", "regime"),
                                trim      = 0.15,
                                max.lag   = 10,
                                criterion = c("aic", "bic", "hq")) {
  # 1. Input validation
  if (!is.data.frame(data)) stop("`data` must be a data.frame")
  if (!is.character(y) || length(y) != 1) stop("`y` must be a single column name")
  if (!is.character(x) || length(x) < 1) stop("`x` must be one or more column names")
  missing <- setdiff(c(y, x), names(data))
  if (length(missing)) stop("Columns not found: ", paste(missing, collapse = ", "))
  shift     <- match.arg(shift)
  criterion <- match.arg(criterion)

  # 2. Ensure breaktest is available
  if (!requireNamespace("breaktest", quietly = TRUE)) {
    message("Installing 'breaktest' from GitHub…")
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
    remotes::install_github("d9d6ka/RANEPA-R")
    if (!requireNamespace("breaktest", quietly = TRUE))
      stop("Failed to install 'breaktest'; please run remotes::install_github('d9d6ka/RANEPA-R')")
  }

  # 3. Extract series
  y_vec  <- as.numeric(data[[y]])
  x_list <- lapply(x, function(col) as.numeric(data[[col]]))

  # 4. Prepare arguments and run the test
  args <- c(
    list(y_vec),
    x_list,
    list(
      shift     = shift,
      trim      = trim,
      max.lag   = max.lag,
      criterion = criterion
    )
  )
  res <- do.call(breaktest::coint.test.GH, args)

  return(res)
}
