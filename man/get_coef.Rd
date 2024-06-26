% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_coef.R
\name{get_coef}
\alias{get_coef}
\title{Aggregated and raw coefficients in a sparse group boosting model}
\usage{
get_coef(sgb_model)
}
\arguments{
\item{sgb_model}{Model of type \code{mboost} to compute the coefficients for.}
}
\value{
List of data.frames containing the a data.frame \code{'$raw'} with the
variable and the raw (Regression) coefficients and the data.frame \code{'$aggregated'} with the
aggregated (Regression) coefficients.
}
\description{
Computes the aggregated coefficients from group and individual baselearners.
Also returns the raw coefficients associated with each baselearner.
}
\details{
in a sparse group boosting models a variable in a dataset can be selected
as an individual variable or as a group. Therefore there can be two associated effect sizes for the
same variable. This function aggregates both and returns it in a data.frame.
}
\examples{
library(mboost)
library(dplyr)
set.seed(1)
df <- data.frame(
  x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100),
  x4 = rnorm(100), x5 = runif(100)
)
df <- df \%>\%
  mutate_all(function(x) {
    as.numeric(scale(x))
  })
df$y <- df$x1 + df$x4 + df$x5
group_df <- data.frame(
  group_name = c(1, 1, 1, 2, 2),
  var_name = c("x1", "x2", "x3", "x4", "x5")
)

sgb_formula <- create_formula(alpha = 0.3, group_df = group_df)
sgb_model <- mboost(formula = sgb_formula, data = df)
sgb_coef <- get_coef(sgb_model)
}
