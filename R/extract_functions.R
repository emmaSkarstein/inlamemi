#' Extract random effects from formula
#'
#' @param formula an object of class "formula", either the formula for the model of interest, the imputation model or the missingness model.
#'
#' @return A list containing "reff_vars", the random effect variables, and "reff", the entire random effect term.
#'
extract_random_effects <- function(formula){
  formula_components <- as.list(formula)
  rhs_terms <- labels(terms(formula))
  reff_vars <- "" # "initialize" in case there are no random effects
  reff <- ""
  # Note: formula_components has a different structure if there is ONLY a random
  # effect, no other covariates.
  # So we need to check if there are multiple terms or only random effect.
  if(length(rhs_terms) > 1){ # Check if there are more than two rhs terms
    rhs_components <- formula_components[[3]]
  }else{
    rhs_components <- formula_components[3]
  }
  reff_index <- which(grepl("f\\((.*)\\)", rhs_components))
  if(length(reff_index) > 0){
    reff_vars <- setdiff(all.names(rhs_components[reff_index][[1]]),
                         c("f", "list", "-", "c"))
    reff <- as.character(rhs_components[reff_index])
  }

  return(list(reff_vars = reff_vars, reff = reff))
}

#' Extract and group variables from formulas
#'
#' Helper function that takes in the formulas for the model of interest and the
#' imputation model, and groups them into responses, covariates, covariate with
#' error and covariate(s) without error, for both sub-models.
#'
#' @inheritParams fit_inlamemi
#'
#' @return A list containing the names of the different variables of the model. The names of the elements in the list are "response_moi" (the response for the moi), "covariates_moi" (all covariates in the moi), "error_variable" (the name of the variable with error or missing data), "covariates_error_free" (the moi covariates without error), "response_imp" (imputation model response), "covariates_imp" (imputation model covariates).
#' @export
#'
#' @importFrom stats terms
#' @importFrom methods is
#'
#' @examples
#' extract_variables_from_formula(formula_moi = y ~ x + z,
#'                                formula_imp = x ~ z)
extract_variables_from_formula <- function(formula_moi,
                                           formula_imp,
                                           formula_mis = NULL,
                                           error_variable = NULL){

  if(!(is(formula_moi, "formula") | is(formula_moi, "list")) |
     !(is(formula_imp, "formula") | is(formula_imp, "list"))){
    stop("One or more of the formulas are not of class 'formula'.")
  }

  if(!is.null(formula_mis) & !(is(formula_mis, "formula") | is(formula_mis, "list"))){
    stop("'formula_mis' is not of class 'formula'.")
  }

  # Model of interest ----------------------------------------------------------

  # Model of interest variables
  moi_components <- as.list(formula_moi)
  # Right hand side terms (string)
  rhs_terms_moi <- labels(terms(formula_moi))

  # MOI random effects ---------------------------------------------------------
  moi_reff <- extract_random_effects(formula_moi)
  reff_vars_moi <- moi_reff$reff_vars

  # MOI covariates -------------------------------------------------------------
  ## select variables that are NOT random effects
  covariates_moi <- setdiff(all.vars(moi_components[[3]]), reff_vars_moi)

  # Error variable -------------------------------------------------------------
  ## Identify error prone variable:
  if(is.list(formula_imp)){
    n_error_var <- length(formula_imp)
    formula_imp_list <- formula_imp
    formula_mis_list <- formula_mis
  }else{
    n_error_var <- 1
    formula_imp_list <- list(formula_imp)
    formula_mis_list <- list(formula_mis)
  }

  error_variables <- c()
  for(i in 1:n_error_var){ # for each error variable
    error_variable_imp <- rlang::as_string(as.list(formula_imp_list[[i]])[[2]])
    error_variables[i] <- error_variable_imp
  }

  if(is.null(error_variable)) error_variable <- error_variables

  # Check if error variable and specified error variable are the same
  if(any(error_variable != error_variables)){
    stop("The specified error variable and the response of the imputation model are not the same.")
  }

  ## Extract index of error variable in formula:
  error_var_index <- which(covariates_moi %in% error_variables)

  ## Error-free covariates:
  covariates_error_free <- covariates_moi[-error_var_index]

  # MOI interaction effects ----------------------------------------------------
  error_interaction_list <- ""
  error_interaction_variables <- ""
  ## Check for interaction effects involving error variable:
  interaction_locs <- grepl(":|\\*", rhs_terms_moi)
  if(any(interaction_locs)){
    # If there are ANY interactions in the formula, we need to check if any of
    #  them involve the error variable.
    # Interactions:
    interactions <- rhs_terms_moi[interaction_locs]
    interaction_matrix <- sapply(error_variables, FUN = grepl, interactions)
    # If there are multiple error variables and only one interaction effect,
    # then the above doesn't become a matrix, so we force it.
    if(length(error_variables) > 1 && is.null(dim(interaction_matrix))){
      dim(interaction_matrix) <- c(1, length(error_variables))
    }
    # Error variables that have an interaction effect:
    if(!is.null(ncol(interaction_matrix))){ # Check if there are multiple error variables?
      error_vars_with_interaction <- error_variables[as.logical(colSums(interaction_matrix))]
    }else{
      error_vars_with_interaction <- error_variables[interaction_matrix]
    }
    # Index of error interactions in list of all interactions
    if(!is.null(nrow(interaction_matrix))){ # Check if there are multiple interactions?
      error_interaction_id <- as.logical(rowSums(interaction_matrix))
    }else{
      error_interaction_id <- interaction_matrix
    }
    if(any(error_interaction_id)){
      error_interactions <- interactions[error_interaction_id]
      error_interaction_list <- strsplit(error_interactions, ":")
      error_interaction_variables <- sapply(error_interaction_list, setdiff, error_variable)
    }
  }

  # Imputation model -----------------------------------------------------------
  # Number of imputation models = no. error variables
  response_imp_list <- c()
  covariates_imp_list <- c()
  random_effects_imp_list <- c()
  random_effect_variables_imp_list <- c()

  for(i in 1:n_error_var){
    response_imp <- error_variable[i]

    imp_components <- as.list(formula_imp_list[[i]])
    rhs_terms_imp <- labels(terms(formula_imp_list[[i]]))

    # Imp. random effects --------------------------------------------------------
    imp_reff <- extract_random_effects(formula_imp_list[[i]])
    reff_vars_imp <- imp_reff$reff_vars
    reff_imp <- imp_reff$reff

    # Imp. covariates ----------------------------------------------------------
    ## Imputation model covariates:
    covariates_imp <- setdiff(all.vars(imp_components[[3]]), reff_vars_imp)

    # Save to lists
    response_imp_list[[i]] <- response_imp
    covariates_imp_list[[i]] <- covariates_imp
    random_effects_imp_list[[i]] <- reff_imp
    random_effect_variables_imp_list[[i]] <- reff_vars_imp
  }

  # Missingness model ----------------------------------------------------------

  # Number of missingness models = no. error variables
  covariates_mis_list <- c()
  covariates_error_free_mis_list <- c()
  random_effects_mis_list <- c()
  random_effect_variables_mis_list <- c()

  for(i in 1:n_error_var){
    if(!is.null(formula_mis_list[[i]])){
      mis_components <- as.list(formula_mis_list[[i]])
      rhs_terms_mis <- labels(terms(formula_mis_list[[i]]))

      # Mis. random effects ---------------------------------------------------------
      mis_reff <- extract_random_effects(formula_mis_list[[i]])
      reff_vars_mis <- mis_reff$reff_vars
      reff_mis <- mis_reff$reff

      # Mis. covariates ----------------------------------------------------------
      ## Missingness model covariates:
      covariates_mis <- setdiff(all.vars(mis_components[[3]]), reff_vars_mis)

      # If missingness model interaction effects with the variable with
      # missingness is to be implemented, this would be done in the same way as
      # for the MOI interaction effects.

      if(error_variables[[i]] %in% covariates_mis){
        ## Extract index of error variable in formula:
        error_var_index <- which(covariates_mis %in% error_variables[[i]])
        ## Error-free covariates:
        covariates_error_free_mis <- covariates_mis[-error_var_index]
      }else{
        covariates_error_free_mis <- covariates_mis
      }


      # Save to lists
      covariates_mis_list[[i]] <- covariates_mis
      covariates_error_free_mis_list[[i]] <- covariates_error_free_mis
      random_effects_mis_list[[i]] <- reff_mis
      random_effect_variables_mis_list[[i]] <- reff_vars_mis
    }else{
      covariates_mis_list[[i]] <- ""
      covariates_error_free_mis_list[[i]] <- ""
      random_effects_mis_list[[i]] <- ""
      random_effect_variables_mis_list[[i]] <- ""
    }
  }

  return(list(
    # Response for the model of interest. Single character or in some cases (such as survival models) a character vector.
    response_moi = all.vars(moi_components[[2]]),
    # Covariates for the model of interest. Character vector.
    covariates_moi = covariates_moi,
    # The actual random effect term(s) in the model of interest. Character vector.
    random_effects_moi = moi_reff$reff,
    # The variable(s) involved in the random effect(s). Character vector.
    random_effect_variables_moi = reff_vars_moi,
    # Error variable(s). List.
    error_variable = error_variable,
    # Interaction effects involving the error variable(s). Length of the list is equal to the number of error variable interaction effects.
    error_interaction_list = error_interaction_list,
    # The non-error variables interacted with the respective error variables. List elements are otherwise the same as in "error_interaction_list".
    error_interaction_variables = error_interaction_variables,
    # Covariates in moi without error
    covariates_error_free = covariates_error_free,
    # Response variable(s) for imputation model(s). List.
    response_imp = response_imp_list,
    # Covariates for each imputation model. List elements are in same order as the responses in response_imp.
    covariates_imp = covariates_imp_list,
    # Random effect(s) in imputation model(s). List elements are in same order as above.
    random_effects_imp = random_effects_imp_list,
    # Variables involved in the random effect(s) in the imputation model(s). Same order as random_effects_imp.
    random_effect_variables_imp = random_effect_variables_imp_list,
    # Covariates for the missingness model
    covariates_mis = covariates_mis_list,
    # error free missingness covariates
    covariates_error_free_mis_list = covariates_error_free_mis_list,
    # Random effects for the missingness model
    random_effects_mis = random_effects_mis_list,
    # Variables involved in the random effects in the missingness model.
    random_effect_variables_mis = random_effect_variables_mis_list#,
    #error_interaction_list_mis = error_interaction_list_mis,
    #error_interaction_variables_mis = error_interaction_variables_mis
    )
  )
}
