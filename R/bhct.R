#' Bayer–Hanck Composite Cointegration Test
#'
#' Combines p‑values from Engle–Granger, Phillips–Ouliaris, and (optionally)
#' Johansen cointegration tests using Fisher’s method.
#'
#' @param data A data.frame or matrix containing all time series.
#' @param vars Character vector: first element is the dependent series, the
#'   rest are regressors.
#' @param lags Integer; lag order for the Johansen test (default 2).
#' @param include_Johansen Logical; include Johansen test in the composite? (default TRUE)
#' @param alpha Numeric; significance level for decision rule (default 0.05).
#' @return A list with:
#'   \itemize{
#'     \item \code{individual_pvalues}: named vector of p‑values,
#'     \item \code{fisher_statistic}, \code{degrees_of_freedom},
#'     \item \code{combined_pvalue}, \code{decision}.
#'   }
#' @export
bhct <- function(data, vars, lags = 2,
                 include_Johansen = TRUE,
                 alpha = 0.05) {
  # Ensure required packages are installed
  required_pkgs <- c("stats", "tseries", "urca")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }

  # 1) Basic checks
  if (!all(vars %in% names(data))) {
    stop("Not all specified variables are found in 'data'.")
  }
  selected_data <- data[, vars, drop = FALSE]
  if (ncol(selected_data) < 2) {
    stop("Please select at least two variables (one dependent and at least one independent)." )
  }

  # 2) Split dependent & independent
  dep   <- selected_data[[1]]
  indep <- as.data.frame(selected_data[,-1, drop = FALSE])

  # (A) Engle–Granger
  eg_model <- stats::lm(dep ~ ., data = indep)
  eg_resid <- stats::resid(eg_model)
  p_EG      <- tseries::adf.test(eg_resid)$p.value
  cat("Engle–Granger p =", p_EG, "\n")

  # (B) Phillips–Ouliaris
  po_res  <- urca::ca.po(as.matrix(selected_data),
                         type   = "Pz",
                         demean = "constant",
                         lag    = "short")
  po_stat <- po_res@teststat[1]
  po_crit <- po_res@cval[1, "5pct"]
  p_PO    <- if (po_stat < po_crit) 0.05 else 1
  cat("Phillips–Ouliaris p =", p_PO, "\n")

  p_values <- c(Engle_Granger = p_EG,
                Phillips_Ouliaris = p_PO)

  # (C) Johansen (optional)
  if (include_Johansen) {
    jo_res  <- urca::ca.jo(as.matrix(selected_data),
                           type  = "trace",
                           ecdet = "const",
                           K     = lags)
    stat_J  <- jo_res@teststat[1]
    crit_J  <- jo_res@cval[1, "5pct"]
    p_J     <- if (stat_J > crit_J) 0.05 else 1
    cat("Johansen p =", p_J, "\n")
    p_values <- c(p_values, Johansen = p_J)
  }

  # (D) Fisher's method
  fisher_stat <- -2 * sum(log(p_values))
  df          <- 2 * length(p_values)
  combined_p  <- 1 - stats::pchisq(fisher_stat, df)
  cat("Fisher χ² =", fisher_stat,
      "; df =", df,
      "; combined p =", combined_p, "\n")

  # (E) Decision
  decision <- if (combined_p < alpha) {
    "Reject null of no cointegration."
  } else {
    "Fail to reject null of no cointegration."
  }
  cat("Decision (alpha =", alpha, "):", decision, "\n")

  invisible(list(
    individual_pvalues = p_values,
    fisher_statistic   = fisher_stat,
    degrees_of_freedom = df,
    combined_pvalue    = combined_p,
    decision           = decision
  ))
}
