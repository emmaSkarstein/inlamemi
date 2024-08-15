#' Extract imputed values
#'
#' @param inlamemi_model object of class `inlamemi`
#' @param error_variable character string indicating the name of the error variable for which to extract the imputed values.
#'
#' @return A list of two objects: the posterior marginal distributions for each element of the imputed covariate, and a data frame giving a summary of these marginals.
#' @export
#'
get_imputed <- function(inlamemi_model, error_variable){
  imputed_marginals <- inlamemi_model$marginals.random[[paste0("id.", error_variable)]]
  imputed_summary <- inlamemi_model$summary.random[[paste0("id.", error_variable)]]

  return(list(imputed_marginals = imputed_marginals, imputed_summary = imputed_summary))
}

#' Extract model coefficients
#'
#' @param object object of class `inlamemi`
#' @param ... other arguments
#'
#' @method coef inlamemi
#' @rdname coef
#' @export
#'
coef.inlamemi <- function(object, ...){
  model_summary <- simplify_inlamemi_model_summary(object)
  all_params <- dplyr::bind_rows(model_summary)

  return(all_params)
}

#' Extract coefficients for the model of interest (MOI)
#'
#' @inheritParams get_imputed
#'
#' @return A data frame with a summary of the posterior marginals for the coefficients in the model of interest.
#' @export
#'
get_coef_moi <- function(inlamemi_model){
  model_summary <- summary(inlamemi_model)

  cat("Formula for model of interest: \n")
  print(model_summary$formula_moi, showEnv = FALSE)
  cat("\n")

  # Select only the beta error coefs
  error_coef <- model_summary$error_coef
  moi_error_coef <- dplyr::filter(error_coef, grepl("beta.", rownames(error_coef)))

  moi_coef <- rbind(model_summary$moi_coef, moi_error_coef)

  return(moi_coef)
}

#' Extract coefficients for the imputation model (IMP)
#'
#' @inheritParams get_imputed
#'
#' @return A data frame with a summary of the posterior marginals for the coefficients in the imputation model.
#' @export
#'
get_coef_imp <- function(inlamemi_model){
  model_summary <- summary(inlamemi_model)

  cat("Formula for imputation model: \n")
  print(model_summary$formula_imp, showEnv = FALSE)
  cat("\n")

  return(model_summary$imp_coef)
}

#' Extract coefficients for the missingness model (MIS)
#'
#' @inheritParams get_imputed
#'
#' @return A data frame with a summary of the posterior marginals for the coefficients in the missingness model.
#' @export
#'
get_coef_mis <- function(inlamemi_model){
  model_summary <- summary(inlamemi_model)

  if(!is.null(model_summary$formula_mis)){
    cat("Formula for missingness model: \n")
    print(model_summary$formula_mis, showEnv = FALSE)
    cat("\n")

    # Select only the beta error coefs
    error_coef <- model_summary$error_coef
    mis_error_coef <- dplyr::filter(error_coef, grepl("gamma.", rownames(error_coef)))

    mis_coef <- rbind(model_summary$mis_coef, mis_error_coef)

    return(mis_coef)
  }else{
    cat("This model has no missingness model.")
    return(NULL)
  }
}

