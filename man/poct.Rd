% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poct.R
\name{poct}
\alias{poct}
\title{Multivariate Phillips–Ouliaris Cointegration Test}
\usage{
poct(
  data,
  vars,
  demean = "constant",
  lags = "short",
  alpha = 0.05,
  type = "Pz"
)
}
\arguments{
\item{data}{A data.frame containing your time series.}

\item{vars}{A character vector of column names to include (length ≥ 2).}

\item{demean}{One of "none", "constant", or "trend" (default "constant").}

\item{lags}{"short", "long", or an integer (default "short").}

\item{alpha}{Significance level for selecting the critical value (0.01, 0.05, or 0.1; default 0.05).}

\item{type}{Test statistic type: "Pz" or "Pu" (default "Pz").}
}
\value{
A list with
\itemize{
\item \code{test_result} – the \code{ca.po()} object
\item \code{test_stat} – the single Phillips–Ouliaris statistic
\item \code{critical_values} – the full named vector of critical values
\item \code{selected_critical_value} – the value matching your \code{alpha}
}
}
\description{
This is a drop‑in replacement for \code{poct()} that lets you test
cointegration among \emph{any} number of series in your data frame.
}
\details{
Combines your selected columns (≥2) into a multivariate \code{ts} matrix and
runs \code{urca::ca.po()}.  If \strong{urca} is not installed, you’ll be prompted
to \code{install.packages("urca")}.
}
