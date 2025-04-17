#' Multivariate Phillips–Ouliaris Cointegration Test
#'
#' Runs `urca::ca.po()` on any number of series and returns:
#'   - `results`: data.frame with test_stat, crit_10, crit_5, crit_1, p_value
'    - `test_object`: the full `ca.po` S4 object (invisible)
#'
#' @param data   data.frame of I(1) series
#' @param vars   character vector of column names (length ≥2)
#' @param demean one of "none","constant","trend" (default "constant")
#' @param lags   "short","long" or integer (default "short")
#' @param type   test type: "Pz" or "Pu" (default "Pz")
#'
#' @return List with:
#'   * `results` – one-row data.frame of statistic, critical values, p_value
#'   * `test_object` – the underlying `ca.po` object (invisible)
#'
#' @export
poct <- function(data, vars,
                 demean = "constant",
                 lags   = "short",
                 alpha  = 0.05,
                 type   = "Pz") {

  # Check urca
  if (!requireNamespace("urca", quietly = TRUE)) {
    stop("Package 'urca' is required. install.packages('urca')")
  }

  # Validate inputs
  if (length(vars) < 2) stop("Supply at least two column names.")
  if (!all(vars %in% names(data))) stop("Some vars not found in data.")

  # Build ts matrix
  series_list <- lapply(vars, function(v) {
    x <- data[[v]]; if (!is.ts(x)) x <- ts(x); x
  })
  ts_mat <- do.call(cbind, series_list)
  colnames(ts_mat) <- vars

  # Determine lag argument for ca.po()
  lag_arg <- if (is.numeric(lags)) {
    warning("Numeric lags not supported by ca.po(); using 'short' instead.")
    "short"
  } else {
    match.arg(lags, c("short","long"))
  }

  # Run PO test
  po <- urca::ca.po(ts_mat,
                    demean = demean,
                    lag    = lag_arg,
                    type   = type)

  # Extract statistic and critical values
  stat <- as.numeric(po@teststat)
  cvs  <- po@cval[1, c("10pct","5pct","1pct")]
  names(cvs) <- c("crit_10","crit_5","crit_1")

  # Compute p-value bound
  pval <- if      (stat > cvs["crit_1"])  "<0.01"
          else if (stat > cvs["crit_5"])  "<0.05"
          else if (stat > cvs["crit_10"]) "<0.10"
          else                             ">0.10"

  # Assemble results
  res_tbl <- data.frame(
    test_stat = stat,
    crit_10   = cvs["crit_10"],
    crit_5    = cvs["crit_5"],
    crit_1    = cvs["crit_1"],
    p_value   = pval,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  invisible(list(
    results     = res_tbl,
    test_object = po
  ))
}
