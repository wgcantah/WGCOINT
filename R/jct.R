#' Johansen Cointegration Test (with critical values & approximate p‑values)
#'
#' Performs the Johansen cointegration test on specified variables in a data frame,
#' and returns a table of test statistics, critical values (10%, 5%, 1%), and
#' a crude p‑value bound for each null rank r = 0,1,2,...
#'
#' @param df A data.frame containing your time series data.
#' @param vars A character vector of column names on which the test will be applied.
#' @param K   Number of lags to include in the VAR (default: 2).
#' @param type Test statistic to use: "trace" (default) or "eigen".
#' @param ecdet  Deterministic trend: "none", "const", "trend", or "both" (default: "const").
#'
#' @return A data.frame with columns:
#'   * `r` – the null hypothesis rank tested (0 vs. >0, 1 vs. >1, …)  
#'   * `test_stat` – the Johansen statistic for that null  
#'   * `crit_10`, `crit_5`, `crit_1` – critical values at 10%, 5%, 1% levels  
#'   * `p_value` – crude bound on the p‑value (e.g. "<0.05", ">0.10")  
#'
#' @examples
#' \dontrun{
#'  set.seed(42)
#'  dat <- data.frame(
#'    x = cumsum(rnorm(200)),
#'    y = cumsum(rnorm(200)),
#'    z = cumsum(rnorm(200))
#'  )
#'  jct_tab <- jct(dat, vars = c("x","y","z"), K = 2)
#'  print(jct_tab)
#' }
#' @export
jct <- function(df, vars, K = 2,
                    type = "trace",
                    ecdet = "const") {

  # 1) Ensure urca is available
  if (!requireNamespace("urca", quietly = TRUE)) {
    install.packages("urca")
  }

  # 2) Input checks
  if (!all(vars %in% names(df))) {
    stop("Some of the specified 'vars' are not in your data frame.")
  }
  sub_df <- df[, vars, drop = FALSE]
  if (!all(sapply(sub_df, is.numeric))) {
    stop("All selected variables must be numeric.")
  }

  # 3) Run Johansen test
  res <- urca::ca.jo(sub_df, type = type, ecdet = ecdet, K = K)

  # 4) Extract test statistics & critical values
  stats <- as.numeric(res@teststat)
  cval  <- res@cval

  # 5) Build results table
  #    r = 0:(n_series - 1)  tests rank <= r vs > r
  n_ranks <- length(stats)
  out <- data.frame(
    r         = 0:(n_ranks - 1),
    test_stat = stats,
    crit_10   = cval[, "10pct"],
    crit_5    = cval[, "5pct"],
    crit_1    = cval[, "1pct"],
    stringsAsFactors = FALSE
  )

  # 6) Approximate p‑value bound
  out$p_value <- apply(out, 1, function(row) {
    stat  <- as.numeric(row["test_stat"])
    cv1   <- as.numeric(row["crit_1"])
    cv5   <- as.numeric(row["crit_5"])
    cv10  <- as.numeric(row["crit_10"])
    if (stat > cv1)  return("<0.01")
    if (stat > cv5)  return("<0.05")
    if (stat > cv10) return("<0.10")
    return(">0.10")
  })

  # 7) Return
  return(out)
}
