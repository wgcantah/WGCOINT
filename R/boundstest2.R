#’ @title Simple ARDL‐Bounds Test (show only cointegration table)
#’ @description
#’   Automatically picks optimal lags (by AIC/BIC/MASE/GMRAE), runs the
#’   Pesaran–Shin–Smith bounds test, and prints *only* the COINTEGRATION TEST
#’   table (no diagnostics, no ECM).
#’
#’ @param data    A data.frame (or something coercible) containing your series.
#’ @param formula An R formula, e.g. y ~ x1 + x2.
#’ @param case    Integer 1–5 (Pesaran‐Shin‐Smith case); default 3 (intercept & trend).
#’ @param max.p   Max AR order to try for the dependent variable; default 5.
#’ @param max.q   Max lag to try for each independent variable; default 5.
#’ @param ic      Which IC to use in the grid‐search: one of "AIC","BIC","MASE","GMRAE".
#’ @param HAC     If TRUE, use Newey–West HAC covariances for the F‐test; default FALSE.
#’ @return Invisibly, the full list that dLagM::ardlBound would return;
#’         printed output is *only* the cointegration table.
#’ @export
boundstest <- function(data,
                       formula,
                       case    = 3,
                       max.p   = 5,
                       max.q   = 5,
                       ic      = c("AIC","BIC","MASE","GMRAE"),
                       HAC     = FALSE) {

  ic <- match.arg(ic)
  if (!requireNamespace("dLagM", quietly=TRUE))
    stop("Please install the dLagM package: install.packages('dLagM')")

  # 1. Coerce & subset to formula vars
  df   <- as.data.frame(data)
  vars <- all.vars(formula)
  missing <- setdiff(vars, names(df))
  if (length(missing))
    stop("Data are missing these variables: ", paste(missing, collapse=", "))
  df <- df[, vars, drop=FALSE]

  # 2. Capture *all* output from ardlBound() with autoOrder=TRUE,
  #    ECM=FALSE & stability=FALSE (so it stops immediately after pssbounds).
  full_out <- utils::capture.output(
    res <- dLagM::ardlBound(
      data      = df,
      formula   = formula,
      case      = case,
      p         = NULL,
      autoOrder = TRUE,
      max.p     = max.p,
      max.q     = max.q,
      ic        = ic,
      HAC       = HAC,
      ECM       = FALSE,
      stability = FALSE
    )
  )

  # 3. Find where the cointegration table starts
  start <- grep("COINTEGRATION TEST", full_out, ignore.case=TRUE)
  if (!length(start)) {
    warning("Could not locate the cointegration table in the output.")
  } else {
    # Print only from that line onwards
    cat(full_out[start:length(full_out)], sep="\n")
  }

  # 4. Return the full result invisibly, in case you need the F‑stat etc.
  invisible(res)
}
