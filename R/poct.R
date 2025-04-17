#' Multivariate Phillips–Ouliaris Cointegration Test (extended)
#'
#' Performs the multivariate Phillips–Ouliaris test on any number of series,
#' and returns a table of test statistic, critical values (10%, 5%, 1%),
#' plus a crude p‑value bound.
#'
#' @param data   A data.frame containing your time series.
#' @param vars   Character vector of at least two column names in `data`.
#' @param demean One of "none", "constant", or "trend" (default "constant").
#' @param lags   "short", "long", or a positive integer (default "short").
#' @param type   Test statistic type: "Pz" (default) or "Pu".
#'
#' @return A list with components:
#'   * `results` – a data.frame with columns  
#'       - `test_stat` – the PO statistic  
#'       - `crit_10`,`crit_5`,`crit_1` – critical values at 10%, 5%, 1%  
#'       - `p_value` – crude p‑value bound  
#'   * `test_object` – the original `ca.po` object, invisibly  
#'
#' @examples
#' \dontrun{
#'   # simulate 3 I(1) series
#'   set.seed(2025)
#'   df <- data.frame(
#'     z1 = cumsum(rnorm(200)),
#'     z2 = cumsum(rnorm(200)),
#'     z3 = cumsum(rnorm(200))
#'   )
#'   out <- poct(df, vars = c("z1","z2","z3"), lags = 4)
#'   print(out$results)
#' }
#' @export
poct<- function(data, vars,
                     demean = "constant",
                     lags   = "short",
                     type   = "Pz") {

  # 1) Package check
  if (!requireNamespace("urca", quietly = TRUE)) {
    stop("Package 'urca' is required. install.packages('urca')")
  }

  # 2) Input validation
  if (length(vars) < 2) {
    stop("Please supply at least two column names in `vars`.")
  }
  if (!all(vars %in% names(data))) {
    stop("Some `vars` not found in `data`.")
  }

  # 3) Build a multivariate ts matrix
  series_list <- lapply(vars, function(v) {
    x <- data[[v]]
    if (!is.ts(x)) x <- ts(x)
    x
  })
  ts_mat <- do.call(cbind, series_list)
  colnames(ts_mat) <- vars

  # 4) Run PO test
  po <- urca::ca.po(ts_mat,
                    demean = demean,
                    lag    = lags,
                    type   = type)

  # 5) Extract statistic & crit vals
  stat <- as.numeric(po@teststat)
  cvs  <- po@cval[1, c("10pct","5pct","1pct")]
  names(cvs) <- c("crit_10","crit_5","crit_1")

  # 6) Compute p‑value bound
  pval <- if      (stat > cvs["crit_1"])  "<0.01"
          else if (stat > cvs["crit_5"])  "<0.05"
          else if (stat > cvs["crit_10"]) "<0.10"
          else                             ">0.10"

  # 7) Assemble results table
  res_tbl <- data.frame(
    test_stat = stat,
    crit_10   = cvs["crit_10"],
    crit_5    = cvs["crit_5"],
    crit_1    = cvs["crit_1"],
    p_value   = pval,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  # 8) Return both table and full object
  invisible(list(
    results     = res_tbl,
    test_object = po
  ))
}
