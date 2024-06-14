#' Simple simulated data
#'
#' A simulated dataset to demonstrate how to model different types of measurement error and missing data using the 'inlamemi' package.
#'
#' @format ## `simple_data`
#' A data frame with 1000 rows and 4 columns:
#' \describe{
#'   \item{y}{Response variable}
#'   \item{x}{Covariate measured with error, both Berkson and classical error and missing observations}
#'   \item{x_true}{Correct version of the covariate with error}
#'   \item{z}{Error free covariate, correlated with x}
#' }
#' @source The dataset is simulated.
"simple_data"

#' Framingham heart study data
#'
#' A data set with observations of heart disease status systolic blood pressure (SBP) and smoking status.
#'
#' @format ## `framingham`
#' A data frame with 641 rows and 4 columns:
#' \describe{
#'   \item{disease}{A binary response, 1 if heart disease, 0 otherwise}
#'   \item{sbp1}{log(SBP - 50) at examination 1 (centered)}
#'   \item{sbp2}{log(SBP - 50) at examination 2 (centered)}
#'   \item{smoking}{Smoking status, 1 if smoking, 0 otherwise.}
#' }
#' @source MacMahon et al. (1990) <https://doi.org/10.1016/0140-6736(90)90878-9>
"framingham"

#' Survival data with repeated systolic blood pressure measurements
#'
#' A dataset containing a repeated blood pressure measurement along with some other variables for participants in the Third National Health and Nutrition Survey (NHANES III), merged with data from the US National Death Index by Ruth H. Keogh and Jonathan Bartlett. For the illustration purposes in this package, we have left out observations where smoking status is missing.
#'
#' @format ## `nhanes_survival`
#' A data frame with 3433 rows and 8 columns:
#' \describe{
#'   \item{sbp1}{systolic blood pressure (standardized), first measurement}
#'   \item{sbp2}{systolic blood pressure (standardized), second measurement}
#'   \item{sex}{sex (0 = female, 1 = male)}
#'   \item{age}{age (standardized)}
#'   \item{smoke}{smoking status (0 = no, 1 = yes)}
#'   \item{diabetes}{diabetes status (0 = no, 1 = yes)}
#'   \item{d}{censoring status (0 = censored, 1 = observed death due to cardiovascular disease)}
#'   \item{t}{time until death due to cardiovascular disease occurs}
#' }
#' @source https://github.com/ruthkeogh/meas_error_handbook
"nhanes_survival"

#' Simulated data with two covariates with classical measurement error
#'
#' A simulated dataset to demonstrate how to set up a model in the case where there are two variables with measurement error.
#'
#' @format ## `two_error_data`
#' A data frame with 1000 rows and 5 columns:
#' \describe{
#'   \item{y}{Response variable}
#'   \item{x1}{Covariate measured with classical error, correlated with z}
#'   \item{x2}{Covariate measured with classical error}
#'   \item{x1_true}{Correct version of x1}
#'   \item{x2_true}{Correct version of x2}
#'   \item{z}{Error free covariate, correlated with x1}
#' }
#' @source The dataset is simulated.
"two_error_data"

#' Simulated data with observation missing at random (MAR)
#'
#' A simulated dataset to demonstrate how to set up a model in the case where there are two variables with measurement error.
#'
#' @format ## `mar_data`
#' A data frame with 1000 rows and 5 columns:
#' \describe{
#'   \item{y}{Response variable}
#'   \item{x}{Observed value of covariate, with almost 20 percent missing}
#'   \item{x_true}{Correct version of x, without missingness}
#'   \item{z1}{Covariate correlated with x}
#'   \item{z2}{Covariate correlated with the missingness of x}
#' }
#' @source The dataset is simulated.
"mar_data"
