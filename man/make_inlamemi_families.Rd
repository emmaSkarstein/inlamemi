% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_functions.R
\name{make_inlamemi_families}
\alias{make_inlamemi_families}
\title{Make vector of likelihood families}
\usage{
make_inlamemi_families(family_moi, inlamemi_stack)
}
\arguments{
\item{family_moi}{a string indicating the likelihood family for the model of interest (the main model).}

\item{inlamemi_stack}{object of type inla.stack}
}
\value{
A vector specifying the likelihood family for each model level.
}
\description{
Make vector of likelihood families
}
\examples{
\dontshow{if (requireNamespace('INLA')) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
simple_stack <- make_inlamemi_stacks(formula_moi = y ~ x + z,
                                     formula_imp = x ~ z,
                                     data = simple_data,
                                     error_type = c("classical"))
make_inlamemi_families(family_moi = "gaussian",
                       inlamemi_stack = simple_stack)
\dontshow{\}) # examplesIf}
}
\keyword{internal}
