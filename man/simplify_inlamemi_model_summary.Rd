% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_functions.R
\name{simplify_inlamemi_model_summary}
\alias{simplify_inlamemi_model_summary}
\title{Simplify the "raw" model summary for printing and plotting}
\usage{
simplify_inlamemi_model_summary(inlamemi_model)
}
\arguments{
\item{inlamemi_model}{the model returned from the fit_inlamemi function.}
}
\value{
A list of four data frames, containing the summaries for different components of the model. These are the coefficients of the model of interest, the coefficient of the variable with error, the coefficients of the imputation model, and the hyperparameters.
}
\description{
Simplify the "raw" model summary for printing and plotting
}
\keyword{internal}
