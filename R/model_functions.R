#' Make formula for measurement error and missing data model
#'
#' @param vars Results from a call to "extract_variables_from_formula" function. If this is not passed as an argument, it is called inside the function.
#' @inheritParams fit_inlamemi
#'
#' @return An object of class "formula".
#' @export
#' @keywords internal
#'
#' @examplesIf requireNamespace('INLA')
#' make_inlamemi_formula(formula_moi = y ~ x + z,
#'                       formula_imp = x ~ z,
#'                       error_type = "classical",
#'                       prior.beta.error = c(0, 1/1000)
#'                       )
make_inlamemi_formula <- function(formula_moi,
                                  formula_imp,
                                  formula_mis = NULL,
                                  family_moi = "gaussian",
                                  error_type = "classical",
                                  error_variable = NULL,
                                  prior.beta.error,
                                  prior.gamma.error = NULL,
                                  vars = NULL){

  # Weaknesses:
  # Fixed size of id.x and id.r values (1:n)

  # Extract and group all variables from formulas:
  if(is.null(vars)){
    vars <- extract_variables_from_formula(formula_moi = formula_moi,
                                           formula_imp = formula_imp,
                                           formula_mis = formula_mis,
                                           error_variable = error_variable)
  }

  # If there are multiple error variables but just one specified error type,
  # then make list with that error type in each element.
  if(length(error_variable) > 1 & !is.list(error_type)){
    nr_error <- length(error_variable)
    error_type <- rep(list(error_type), nr_error)
  }

  # Define covariates in output formula as all variables in moi formula
  #  except the response and error prone covariate:
  if(length(vars$covariates_error_free > 0)){
    covariates_error_free_string <- paste0("beta.", vars$covariates_error_free)
  }else{
    covariates_error_free_string <- ""
  }

  # Check the prior for beta.x
  # If only one prior is given, put it in a list so the indexation turns out correctly
  if(!is.list(prior.beta.error)){
    prior.beta.error <- list(prior.beta.error)
  }
  if(length(prior.beta.error) < length(vars$error_variable)){
    prior.beta.error <- lapply(1:length(vars$error_variable),
                               function(x) prior.beta.error[[1]])
    # Maybe add warning here?
  }

  # String for potential interaction effects
  interaction_effects <- ""
  if(vars$error_interaction_list != ""){
    for(i in length(vars$error_interaction_list)){
      non_error_var <- vars$error_interaction_variables[[i]]
      error_var <- setdiff(vars$error_interaction_list[[i]], non_error_var)

      coef_name <- paste0("beta.", non_error_var, error_var)
      weight_name <- paste0("weight.", non_error_var, error_var)
      f_string <- paste0("f(", coef_name, ", ", weight_name,
                         ", copy = 'id.", error_var,
                         "', hyper = list(beta = list(param = ",
                         deparse(prior.beta.error[[1]]), ", fixed = FALSE)))")
      interaction_effects <- c(interaction_effects, f_string)
    }
  }

  # Covariates for imputation model(s):
  covariates_imp_string <- c()
  for(i in 1:length(vars$error_variable)){ # For each error variable
    if(length(vars$covariates_imp[[i]]) > 0){
      covariates_imp <- paste0("alpha.", vars$error_variable[i], ".", vars$covariates_imp[[i]])
    }else{
      covariates_imp <- ""
    }
    intercept_imp <- paste0("alpha.", vars$error_variable[i], ".0")
    covariates_imp_string <- c(covariates_imp_string, intercept_imp, covariates_imp)
  }

  # In case there are multiple error variables, the missingness formula
  # will be a list. if only one, we still put the formula in a list so we can
  # treat the two cases the same.
  if(is.list(formula_mis)){
    formula_mis_list <- formula_mis
  }else{
    if(is.null(formula_mis)){
      formula_mis_list <- rep(list(NULL), length(vars$error_variable))
    }else{
      formula_mis_list <- list(formula_mis)
    }
  }

  # Covariates for missingness model(s)
  covariates_mis_string <- c()
  for(i in 1:length(vars$error_variable)){ # For each error variable
    if(!is.null(formula_mis_list[[i]])){
      if(length(vars$covariates_mis[[i]]) > 0){
        covariates_mis <- paste0("gamma.", vars$error_variable[i], ".",
                                 vars$covariates_error_free_mis_list[[i]])
      }else{
        covariates_mis <- ""
      }
      intercept_mis <- paste0("gamma.", vars$error_variable[i], ".0")
      covariates_mis_string <- c(covariates_mis_string, intercept_mis, covariates_mis)
    }
  }

  # Check the prior for gamma.x
  # If only one prior is given, put it in a list so the indexation turns out correctly
  if(!is.list(prior.gamma.error)){
    prior.gamma.error <- list(prior.gamma.error)
  }
  if(length(prior.gamma.error) < length(vars$error_variable)){
    prior.gamma.error <- lapply(1:length(vars$error_variable),
                               function(x) prior.gamma.error[[1]])
  }


  # The copy term to ensure the mismeasured variable(s) is copied correctly through the models
  copy_terms <- c()
  for(i in 1:length(vars$error_variable)){ # For each error variable, we need these terms
    beta.x.term <- paste0("f(", paste0("beta.", vars$error_variable[[i]]),
                         ", copy = 'id.", vars$error_variable[[i]], "', hyper = list(beta = list(param = ",
                           deparse(prior.beta.error[[i]]), ", fixed = FALSE)))")
    id.x.term <- paste0("f(id.", vars$error_variable[[i]], ", weight.", vars$error_variable[[i]],
                         ", model='iid', values = 1:n, hyper = list(prec = list(initial = -15, fixed=TRUE)))")
    copy_terms <- c(copy_terms, beta.x.term, id.x.term)
    # In case there are multiple error variables, the missingness formula
    # is only defined for the relevant ones
    if(!is.null(formula_mis_list[[i]])){
      gamma.x.term <- paste0("f(", paste0("gamma.", vars$error_variable[[i]]),
                             ", copy = 'id.", vars$error_variable[[i]], "', hyper = list(beta = list(param = ",
                             deparse(prior.gamma.error[[i]]), ", fixed = FALSE)))")
      copy_terms <- c(copy_terms, gamma.x.term)

    }
  }

  # Setting up the response formula
  if(!family_moi %in% inla_survival_families()){
    y_moi <- "y_moi, "
  }else{
    y_moi <- "inla.surv(time = y_time, event = y_event), "
  }

  response_list <- c()
  # Loop over error variables. For each make a classical and imputation response,
  # and optionally Berkson as well
  for(i in 1:length(vars$error_variable)){
    if("berkson" %in% error_type[[i]]){
      berkson_response <- paste0(vars$error_variable[[i]], "_berkson")
      response_list <- c(response_list, berkson_response)
    }

    classical_response <- paste0(vars$error_variable[[i]], "_classical")
    imputation_response <- paste0(vars$error_variable[[i]], "_imp")
    response_list <- c(response_list, classical_response, imputation_response)

    if(!is.null(formula_mis_list[[i]])){
      missingness_response <- paste0(vars$error_variable[[i]], "_mis")
      response_list <- c(response_list, missingness_response)
    }
  }

  error_response_string <- paste(response_list, collapse = ", ")
  response_string <- paste0("list(", y_moi, error_response_string, ")")

  formula_RHS <- paste(
    # Remove default intercept
    "-1",
    # MOI covariates
    "beta.0",
    paste(covariates_error_free_string, collapse = " + "),
    # MOI random effects
    paste(vars$random_effects_moi, collapse = " + "),
    # MOI interaction terms with error variable
    paste(interaction_effects, collapse = " + "),
    # Imputation model covariates
    paste(covariates_imp_string, collapse = " + "),
    # Imputation model random effects
    paste(vars$random_effects_imp, collapse = " + "),
    # Missingness model covariates
    paste(covariates_mis_string, collapse = " + "),
    # Missingness model random effects
    paste(vars$random_effects_mis, collapse = " + "),
    # Missingness model interaction effects
    # TODO
    # Copy terms
    paste(copy_terms, collapse = " + "),
    # Between each term add a "+"
    sep = " + ")

  # Remove duplicate "+"'s
  formula_RHS <- gsub(pattern = "((\\+)( *)){2,}", replacement = "+ ",
                      formula_RHS)

  # Optionally add Berkson layer
  if("berkson" %in% error_type){
    r_term <- "f(id.r, weight.r, model='iid', values = 1:n, hyper = list(prec = list(initial = -15, fixed = TRUE)))"
    formula_RHS <- paste(formula_RHS, "+", r_term)
  }


  formula <- paste(response_string, "~", formula_RHS)

  return(stats::as.formula(formula))
}

#' Make data stacks for joint model specification in INLA
#'
#' @inheritParams fit_inlamemi
#' @inheritParams make_inlamemi_formula
#'
#' @return An object of class inla.stack with data structured according to specified formulas and error models.
#' @export
#' @keywords internal
#'
#' @examplesIf requireNamespace('INLA')
#' make_inlamemi_stacks(formula_moi = y ~ x + z,
#'                    formula_imp = x ~ z,
#'                    data = simple_data,
#'                    error_type = "classical")
make_inlamemi_stacks <- function(formula_moi,
                                 formula_imp,
                                 formula_mis = NULL,
                                 family_moi = "gaussian",
                                 data,
                                 error_type = "classical",
                                 error_variable = NULL,
                                 repeated_observations = FALSE,
                                 vars = NULL){

  # Extract and group all variables from formulas:
  if(is.null(vars)){
    vars <- extract_variables_from_formula(formula_moi = formula_moi,
                                           formula_imp = formula_imp,
                                           formula_mis = formula_mis,
                                           error_variable = error_variable)
  }

  # Some checks ----
  if("error_variable" %in% vars$error_variable){
    stop("Please name the variable with error something other than 'error_variable', as this name is used internally in the code and will lead to errors.")
  }

  # Check that variables are named correctly in case of repeated measurements
  if(repeated_observations){
    expected_data_variable_names <- paste0(vars$error_variable, c(1,2))
    if(!all(expected_data_variable_names %in% names(data))){
      stop("It looks like you have repeated measurements, and that the formula or data may not be specified correctly. When fitting the model with repeated observations for the variable with missingness or measurement error, make sure that you specify the variable with its general name in the formula, but provide numbered versions in the data, such that each repeat is labelled with a number after the variable name. For instance, assume we have repeated measurements of systolic blood pressure (sbp). Then we would write 'sbp' in the formula, but in the data they would be labelled 'sbp1' and 'sbp2'.")
    }
  }

  # Check if it looks like there may be repeated measurements
  repeated_obs_warning <- FALSE
  for(error_var in vars$error_variable){
    if(sum(grepl(paste0("\\b", error_var, "(\\d)\\b"),
                 names(data)))>1 &&
       !repeated_observations){
      repeated_obs_warning <- TRUE
      warning("It looks like you may have repeated measurements of the variable with measurement error or missingness. If this is the case, specify the argument 'repeated_observations = TRUE' to ensure correct analysis.")
    }
  }


  # Check that all variables in formula exist in data
  formula_vars <- union(all.vars(formula_moi), all.vars(formula_imp))
  data_vars <- names(data)
  if(repeated_observations | repeated_obs_warning){
    # If reason to believe repeated measurements, ignore the error variable in the comparison
    formula_vars <- setdiff(formula_vars, vars$error_variable)
  }
  diff_vars <- setdiff(formula_vars, data_vars) # vars in formula but not in data
  if(length(diff_vars)>0){
    stop(paste0("It looks like the following variable(s) are in the formula but not the data: ", toString(diff_vars)))
  }
  # Check if error variable is not in data
  for(error_var in vars$error_variable){
    if(!repeated_observations && !error_var %in% data_vars){
      stop("The error variable, ", vars$error_variable, ", is not found among the variables in the data.")
    }
  }



  # Defining sizes -------------------------------------------------------------
  n <- nrow(data)

  # Model of interest stack  ---------------------------------------------------

  # Set up intercept vector for MOI:
  beta.0 <- rep(1, n)

  # Set up vector for variable with error
  error_var_coefs <- paste0("beta.", vars$error_variable)
  for(error_var in vars$error_variable){
    beta.error_variable <- paste0("beta.", error_var)
    assign(beta.error_variable, 1:n)
  }


  # Set up vectors for all error-free covariates with the correct names:
  cov_moi_names <- c()

  for(variable in vars$covariates_error_free){
    var_name <- paste0("beta.", variable)
    cov_moi_names <- c(cov_moi_names, var_name)
    assign(var_name, as.matrix(data[variable]))
  }

  # The names of the variables that change based on the covariate names:
  moi_effects <- paste0(c("beta.0", error_var_coefs, cov_moi_names), " = ",
                        c("beta.0", error_var_coefs, cov_moi_names),
                        collapse = ", ")

  # Add (optional) random effect variable
  if(nchar(vars$random_effects_moi) > 0){
    # "list", "-" and "c" will be recognized as variables... so remove them
    assign(vars$random_effect_variables_moi, as.matrix(data[vars$random_effect_variables_moi]))

    reff_list_moi <- paste0(vars$random_effect_variables_moi, " = ", vars$random_effect_variables_moi)
    moi_effects <- paste0(moi_effects, ", ", reff_list_moi)
  }

  # Add (optional) interaction variable
  if(nchar(vars$error_interaction_variables) > 0){
    for(interaction_var in vars$error_interaction_variables){
      coef_name <- paste0("beta.", interaction_var, vars$error_variable)
      assign(coef_name, 1:n)
      weight_name <- paste0("weight.", interaction_var, vars$error_variable)
      assign(weight_name, c(as.matrix(data[interaction_var])))

      interaction_list <- paste0(coef_name, " = ", coef_name)
      weight_list <- paste0(weight_name, " = ", weight_name)
      moi_effects <- paste0(moi_effects, ", ", interaction_list,
                            ", ", weight_list)
    }
  }

  moi_effects_list <- NULL # Assign NULL to moi_effects_list, just to avoid notes when running checks.

  # A string containing the code needed to define a list of all the objects:
  moi_code <- paste0("moi_effects_list <- list(", moi_effects, ")")

  # Evaluate the string of code from above:
  eval(parse(text = moi_code))

  # Response
  ## if survival model, structure the response accordingly
  if(family_moi %in% inla_survival_families()){
    time_var <- vars$response_moi[1]
    event_var <- vars$response_moi[2]
    response_moi <- list(y_time = as.matrix(data[time_var]),
                         y_event = as.matrix(data[event_var]))
  }else{ ## otherwise response is just one vector
    response_moi <- list(y_moi = as.matrix(data[vars$response_moi]))
  }
  # y = x + z...
  stk_moi <- INLA::inla.stack(data = response_moi,
                        A = list(1),
                        effects = list(moi_effects_list),
                        tag = "moi")

  # Empty list to store the error and imputation stacks.
  error_stacks <- list()
  if(is.list(error_type)){
    error_type_list <- error_type
  }else{
    error_type_list <- list(error_type)
  }

  # For each eror variable, either make a Berkson stack or a classical and imputation stack:
  for(i in 1:length(vars$error_variable)) {
    # Berkson stack --------------------------------------------------------------

    if("berkson" %in% error_type_list[[i]]){
      # 0 = -x_true + r + u_b
      # Berkson effects
      id_name <- paste0("id.", vars$error_variable[[i]])
      weight_name <- paste0("weight.", vars$error_variable[[i]])

      berkson_effects_list <- NULL # Assign NULL, just to avoid notes when running checks.
      berkson_code <- paste0("berkson_effects_list <- list(", id_name,
                               " = 1:n, ", weight_name, " = -1)")
      eval(parse(text = berkson_code))
      berkson_effects_list$id.r <- 1:n
      berkson_effects_list$weight.r <- 1

      # Berkson response
      response_berkson <- NULL
      berkson_tag <- paste0(vars$error_variable[[i]], "_berkson")
      response_berkson_code <- paste0("response_berkson <- list(", berkson_tag, "= rep(0,n))")
      eval(parse(text = response_berkson_code))

      stk_b <- INLA::inla.stack(data = response_berkson,
                                A = list(1),
                                effects = list(berkson_effects_list),
                                tag = berkson_tag)

      # Add Berkson stack to list of error stacks
      stk_b_name <- paste0("stk_b_", vars$error_variable[[i]])
      stk_b_code <- paste0("error_stacks <- c(error_stacks, ", stk_b_name, " = list(stk_b))")
      eval(parse(text = stk_b_code))
    }


    # Classical stack  -----------------------------------------------------------

    # Set "height" of measurement error stack depending on no of repeats
    if(repeated_observations){
      error_var_list <- names(data)[grepl(paste0("\\b", vars$error_variable[[i]], "(\\d)\\b"), names(data))]
      nr_repeats <- length(error_var_list)
    }else{
      error_var_list <- vars$error_variable[[i]]
      nr_repeats <- 1
    }

    classical_id <- rep(1:n, nr_repeats)

    # Latent variable r if Berkson ME, otherwise x
    if("berkson" %in% error_type_list[[i]]){
      classical_effects_list <- list(id.r = classical_id, weight.r = 1)
    }else{
      id_name <- paste0("id.", vars$error_variable[[i]])
      weight_name <- paste0("weight.", vars$error_variable[[i]])

      classical_effects_list <- NULL # Assign NULL, just to avoid notes when running checks.
      # A string containing the code needed to define a list of all the objects:
      classical_code <- paste0("classical_effects_list <- list(", id_name,
                               " = classical_id, ", weight_name, " = 1)")
      eval(parse(text = classical_code))

      #classical_effects_list <- list(id.x = classical_id, weight.x = 1)
    }

    # Response
    response_classical <- NULL
    classical_response <- as.matrix(utils::stack(data[error_var_list])[1])

    classical_tag <- paste0(vars$error_variable[[i]], "_classical")
    response_classical_code <- paste0("response_classical <- list(",
                                      classical_tag,
                                      "= classical_response)")
    eval(parse(text = response_classical_code))
    #response_classical <- list(y_classical = classical_response)

    # x = r + u_c
    stk_c <- INLA::inla.stack(data = response_classical,
                              A = list(1),
                              effects = list(classical_effects_list),
                              compress = FALSE,
                              tag = classical_tag)

    # Add classical stack to list of stacks
    stk_c_name <- paste0("stk_c_", vars$error_variable[[i]])
    stk_c_code <- paste0("error_stacks <- c(error_stacks, ", stk_c_name, " = list(stk_c))")
    eval(parse(text = stk_c_code))


    # Imputation stack  ----------------------------------------------------------

    # Intercept for imputation model:
    intercept_name <- paste0("alpha.", vars$error_variable[[i]], ".0")
    assign(intercept_name, rep(1, n))

    # Set up vectors for covariates in the imputation model
    cov_imp_names <- c()

    for(variable in vars$covariates_imp[[i]]){
        var_name <- paste0("alpha.", vars$error_variable[[i]], ".", variable)
        cov_imp_names <- c(cov_imp_names, var_name)
        assign(var_name, as.matrix(data[variable]))
    }

    # The names of the variables that change based on the covariate names:
    imp_effects <- paste0(c(intercept_name, cov_imp_names), " = ",
                          c(intercept_name, cov_imp_names), collapse = ", ")

    # Add (optional) random effect variable
    if(nchar(vars$random_effects_imp[[i]]) > 0){
      assign(vars$random_effect_variables_imp[[i]], as.matrix(data[vars$random_effect_variables_imp[[i]]]))
      reff_list_imp <- paste0(vars$random_effect_variables_imp[[i]], " = ", vars$random_effect_variables_imp[[i]])
      imp_effects <- paste0(imp_effects, ", ", reff_list_imp)
    }

    imp_effects_list <- NULL # Assign NULL to imp_effects_list, just to avoid notes when running checks.

    # A string containing the code needed to define a list of all the objects:
    imp_code <- paste0("imp_effects_list <- list(", imp_effects, ")")

    # Evaluate the string of code from above:
    eval(parse(text = imp_code))

    # Latent variable r if Berkson ME, otherwise x
    if("berkson" %in% error_type){
      imp_effects_list$id.r <- 1:n
      imp_effects_list$weight.r <- rep(-1, n)
    }else{
      id_code <- paste0("imp_effects_list$", id_name, " <- 1:n")
      weight_code <- paste0("imp_effects_list$", weight_name, " <- rep(-1, n)")
      #imp_effects_list$id.x <- 1:n
      #imp_effects_list$weight.x <- rep(-1, n)
      eval(parse(text = id_code))
      eval(parse(text = weight_code))
    }

    # Response
    response_imputation <- NULL
    imputation_tag <- paste0(vars$error_variable[[i]], "_imp")
    response_imputation_code <- paste0("response_imputation <- list(", imputation_tag, "= rep(0,n))")
    eval(parse(text = response_imputation_code))
    #response_imputation <- list(y_imp = rep(0, n))

    # r = z + ...
    stk_imp <- INLA::inla.stack(data = response_imputation,
                                A = list(1),
                                effects = list(imp_effects_list),
                                tag = imputation_tag)

    # Add imputation stack to list of stacks
    stk_imp_name <- paste0("stk_imp_", vars$error_variable[[i]])
    stk_imp_code <- paste0("error_stacks <- c(error_stacks, ", stk_imp_name, " = list(stk_imp))")
    eval(parse(text = stk_imp_code))

    # Missingness stack --------------------------------------------------------
    if(is.list(formula_mis)){
      formula_mis_list <- formula_mis
    }else{
      if(is.null(formula_mis)){
        formula_mis_list <- rep(list(NULL), length(vars$error_variable))
      }else{
        formula_mis_list <- list(formula_mis)
      }
    }
    if(!is.null(formula_mis_list[[i]])){
      # Intercept for missingness model:
      intercept_name <- paste0("gamma.", vars$error_variable[[i]], ".0")
      assign(intercept_name, rep(1, n*nr_repeats))

      # Set up vectors for covariates in the missingness model
      cov_mis_names <- c()

      for(variable in vars$covariates_error_free_mis_list[[i]]){
        var_name <- paste0("gamma.", vars$error_variable[[i]], ".", variable)
        cov_mis_names <- c(cov_mis_names, var_name)
        assign(var_name, rep(as.matrix(data[variable]), nr_repeats))
      }

      # Vector for error_prone variable
      error_var_coef <- paste0("gamma.", vars$error_variable[[i]])
      for(error_var in vars$error_variable[[i]]){
        gamma.error_variable <- paste0("gamma.", error_var)
        assign(gamma.error_variable, rep(1:n, nr_repeats))
      }

      # The names of the variables that change based on the covariate names:
      mis_effects <- paste0(
        c(intercept_name, cov_mis_names, gamma.error_variable), " = ",
        c(intercept_name, cov_mis_names, gamma.error_variable), collapse = ", ")

      # Add (optional) random effect variable
      if(nchar(vars$random_effects_mis[[i]]) > 0){
        assign(vars$random_effect_variables_mis[[i]],
               rep(as.matrix(data[vars$random_effect_variables_mis[[i]]]), nr_repeats))
        reff_list_mis <- paste0(vars$random_effect_variables_mis[[i]], " = ", vars$random_effect_variables_mis[[i]])
        mis_effects <- paste0(mis_effects, ", ", reff_list_mis)
      }

      mis_effects_list <- NULL # Assign NULL to imp_effects_list, just to avoid notes when running checks.

      # A string containing the code needed to define a list of all the objects:
      mis_code <- paste0("mis_effects_list <- list(", mis_effects, ")")

      # Evaluate the string of code from above:
      eval(parse(text = mis_code))


      # # Latent variable r if Berkson ME, otherwise x
      # if("berkson" %in% error_type){ # TODO: Need to check this
      #   mis_effects_list$id.r <- 1:n
      #   mis_effects_list$weight.r <- rep(1, n)
      # }else{
      #   id_code <- paste0("mis_effects_list$", id_name, " <- 1:n")
      #   weight_code <- paste0("mis_effects_list$", weight_name, " <- rep(1, n)")
      #   eval(parse(text = id_code))
      #   eval(parse(text = weight_code))
      # }

      # Response
      response_missingness <- NULL
      missingness_tag <- paste0(vars$error_variable[[i]], "_mis")
      classical_response <- as.matrix(utils::stack(data[error_var_list])[1])

      # In case repeated observations, y_mis may need to be stacked in the same way as the classical response.
      y_mis <- as.numeric(is.na(as.matrix(utils::stack(data[error_var_list])[1])))
      response_missingness_code <- paste0("response_missingness <- list(", missingness_tag, "= y_mis)")
      eval(parse(text = response_missingness_code))

      # m = x + ...
      stk_mis <- INLA::inla.stack(data = response_missingness,
                                  A = list(1),
                                  effects = list(mis_effects_list),
                                  compress = FALSE,
                                  tag = missingness_tag)

      # Add imputation stack to list of stacks
      stk_mis_name <- paste0("stk_mis_", vars$error_variable[[i]])
      stk_mis_code <- paste0("error_stacks <- c(error_stacks, ", stk_mis_name, " = list(stk_mis))")
      eval(parse(text = stk_mis_code))
    }


  } # End loop over error variables


  stk_full <- NULL
  error_stack_names <- names(error_stacks)
  error_stack_names_string <- paste0("error_stacks$", error_stack_names, collapse = ", ")
  stk_full_code <- paste0("stk_full <- INLA::inla.stack(stk_moi, ",
                          error_stack_names_string, ", compress = FALSE)")

  eval(parse(text = stk_full_code))

  return(stk_full)
}

#' Make vector of likelihood families
#'
#' @inheritParams fit_inlamemi
#' @param inlamemi_stack object of type inla.stack
#'
#' @export
#' @keywords internal
#'
#' @return A vector specifying the likelihood family for each model level.
#' @examplesIf requireNamespace('INLA')
#' simple_stack <- make_inlamemi_stacks(formula_moi = y ~ x + z,
#'                                      formula_imp = x ~ z,
#'                                      data = simple_data,
#'                                      error_type = c("classical"))
#' make_inlamemi_families(family_moi = "gaussian",
#'                        inlamemi_stack = simple_stack)
make_inlamemi_families <- function(family_moi, inlamemi_stack){
  level_names <- names(inlamemi_stack$data$names)
  model_names <- sub(".*_", "", level_names)
  likelihood_names <- rep(NA, length(model_names))

  likelihood_names[model_names=="moi"] <- family_moi
  likelihood_names[model_names %in% c("classical", "berkson", "imp")] <- "gaussian"
  likelihood_names[model_names == "mis"] <- "binomial"

  # If survival model the two first entries are now NA.
  if(family_moi %in% inla_survival_families()){
    likelihood_names <- likelihood_names[2:length(likelihood_names)]
    likelihood_names[1] <- family_moi
  }

  return(likelihood_names)
}

#' Construct scaling vector to scale the precision of correctly observed observations
#'
#' @inheritParams fit_inlamemi
#' @inheritParams make_inlamemi_formula
#' @param inlamemi_stack an object of class inlamemi.data.stack containing data structured
#'
#' @return A vector reflecting the scaling factor for the residual terms in each model level.
#' @export
#' @keywords internal
#'
#' @examplesIf requireNamespace('INLA')
#' stacks <- make_inlamemi_stacks(data = simple_data,
#'                              formula_moi = y ~ x + z,
#'                              formula_imp = x ~ z,
#'                              error_type = c("classical", "berkson"))
#' vars <- extract_variables_from_formula(formula_moi = y ~ x + z,
#'                                        formula_imp = x ~ z)
#' make_inlamemi_scaling_vector(stacks,
#'                            error_type = c("classical", "berkson"),
#'                            vars = vars)
make_inlamemi_scaling_vector <- function(inlamemi_stack,
                                         error_type,
                                         classical_error_scaling = NULL,
                                         vars){
  # Scale the classical error precision of the perfectly measured values to a very high value (10^12).
  # If we have only missing data/perfectly observed data, then the precision can be scaled for all (because it makes no difference for the missing values)
  # but if we have measurement error (possibly varying), then the value of the precision is more meaningful.

  stack_index <- inlamemi_stack$data$index
  tags <- names(stack_index)
  tags_classical <- tags[grepl("classical", tags)]
  tags_imp <- tags[grepl("imp", tags)]

  index_moi <- inlamemi_stack$data$index$moi
  index_classical <- inlamemi_stack$data$index[tags_classical]
  index_imp <- inlamemi_stack$data$index[tags_imp]

  n_total <- nrow(inlamemi_stack$data$data)

  # Default case:
  scale_vec <- rep(1, n_total)

  if(!is.list(error_type)){
    error_type <- list(error_type)
  }

  for(i in length(vars$error_variable)){
    if(!"classical" %in% error_type[[i]]){
      # If there is no classical measurement error, then the classical error
      # precision should be scaled very high to indicate that there is no error.
      # Even if there is missingness, this scaling should be done, as the missing
      # values will anyway be imputed.
      tag <- paste0(vars$error_variable[[i]], "_classical")
      index <- index_classical[[tag]]
      scale_vec[index] <- 10^8
    }
  }

  # The manual classical error scaling currently only works if there is just one error variable.
  if(!is.null(classical_error_scaling)){
    n_classical <- length(index_classical[[1]])
    if(length(classical_error_scaling) != n_classical){
      stop(paste0("The length of classical_error_scaling (",
                  length(classical_error_scaling),
                  ") is not the same as the number of observations (",
                  n_classical,
                  ")."))
    }
    # This can be used if the error varies for each observation somehow.
    scale_vec[index_classical[[1]]] <- classical_error_scaling
  }

  return(scale_vec)
}


#' Make "control.family" argument for passing to the "inla" function
#'
#' @inheritParams fit_inlamemi
#'
#' @return the "control.family" argument to be passed to inla, a list of "control.family" arguments for each model in the hierarchical measurement error model.
#' @export
#' @keywords internal
#'
#' @examplesIf requireNamespace('INLA')
#' make_inlamemi_control.family(
#'   family_moi = "gaussian",
#'   error_type = c("berkson", "classical"),
#'   prior.prec.moi = c(10, 9),
#'   prior.prec.berkson = c(10, 9),
#'   prior.prec.classical = c(10, 9),
#'   prior.prec.imp = c(10, 9),
#'   initial.prec.moi = 1,
#'   initial.prec.berkson = 1,
#'   initial.prec.classical = 1,
#'   initial.prec.imp = 1)
#'
#' make_inlamemi_control.family(
#'   family_moi = "weibull.surv",
#'   error_type = c("classical", "missing"),
#'   control.family.moi =
#'     list(hyper = list(alpha = list(param = 0.01,
#'                                    initial = log(1.4),
#'                                    fixed = FALSE))),
#'   prior.prec.classical = c(0.5, 0.5),
#'   prior.prec.imp = c(0.5, 0.5),
#'   initial.prec.classical = 2.8,
#'   initial.prec.imp = 1)
make_inlamemi_control.family <- function(
    formula_mis = NULL,
    family_moi,
    error_type = "classical",
    prior.prec.moi = NULL,
    prior.prec.berkson = NULL,
    prior.prec.classical = NULL,
    prior.prec.imp = NULL,
    initial.prec.moi = NULL,
    initial.prec.berkson = NULL,
    initial.prec.classical = NULL,
    initial.prec.imp = NULL,
    control.family.moi = NULL,
    control.family.berkson = NULL,
    control.family.classical = NULL,
    control.family.imp = NULL,
    control.family = NULL){
  # Define some stuff
  model_levels_without_moi <- union(error_type, c("classical", "imp")) # Always need classical
  model_levels_without_moi_and_missing <- setdiff(model_levels_without_moi, "missing")

  # If control.family is specified, just use that directly.
  if(!is.null(control.family)){
    return(control.family)
  }
  # Otherwise:

  # Check if any defaults are needed -------------------------------------------
  prec.list <- list(moi = prior.prec.moi, berkson = prior.prec.berkson,
                    classical = prior.prec.classical, imp = prior.prec.imp)
  # Not so easy to find indexes of NULL elements in list.
  # Workaround is to find elements that have length 0.
  which_prec_null <- names(prec.list[lengths(prec.list)==0])

  # Find out which of these are missing:
  which_need_defaults <- intersect(which_prec_null, model_levels_without_moi)

  # Set defaults for necessary priors that have not been specified
  for(component in which_need_defaults){
    #warning(paste0("Using default prior Gamma(10, 9) for the precision of the ",
    #               component, " model, and the initial value is set to 1 for the same term. This should be given a better value by specifying 'prior.prec.",
    #               component, "' and 'initial.prec.", component, "'.\n"))
    prior_code <- paste0("prior.prec.", component, " <- c(10, 9); ",
                         "initial.prec.", component, " <- 1")
    eval(parse(text = prior_code))
  }

  # MOI control.family --------------------------------------------------
  if(is.null(control.family.moi)){ # If control.family.moi is not provided
    if(family_moi %in% c("binomial", "poisson")){
      control.family.moi <- list(hyper = list())
    }
    if(family_moi == "gaussian"){
      # Check if default is needed
      if(any(is.null(initial.prec.moi), is.null(prior.prec.moi))){
        warning("Using default prior Gamma(10, 9) for the precision of the model of interest, and the initial value is set to 1 for the same term. This should be given a better value by specifying 'prior.prec.moi' and 'initial.prec.moi'.\n")
        prior.prec.moi <- c(10, 9)
        initial.prec.moi <- 1
      }
      control.family.moi <- list(hyper = list(prec = list(initial = log(initial.prec.moi),
                                                          param = prior.prec.moi,
                                                          fixed = FALSE)))
    }
  }
  control.family <- list(control.family.moi)

  # Berkson control.family (optional) -----------------------------------
  if("berkson" %in% error_type){
    if(is.null(control.family.berkson)){ # If no control.fam.berk provided, insert arguments
      control.family.berkson <- list(hyper = list(
        prec = list(initial = log(initial.prec.berkson),
                    param = prior.prec.berkson,
                    fixed = FALSE)))
    }
    control.family <- append(control.family, list(control.family.berkson))
  }

  # Classical control.family --------------------------------------------
  if(is.null(control.family.classical)){
    control.family.classical <- list(hyper = list(
      prec = list(initial = log(initial.prec.classical),
                  param = prior.prec.classical,
                  fixed = FALSE)))
  }
  control.family <- append(control.family, list(control.family.classical))

  # Imputation control.family --------------------------------------------------
  if(is.null(control.family.imp)){
    control.family.imp <- list(hyper = list(
      prec = list(initial = log(initial.prec.imp),
                  param = prior.prec.imp,
                  fixed = FALSE)))
  }
  control.family <- append(control.family, list(control.family.imp))

  # Missingness control.family (optional) --------------------------------------
  if(!is.null(formula_mis)){
    control.family.mis <- list()
    control.family <- append(control.family, list(control.family.mis))
  }

  return(control.family)
}

#' Fit model for measurement error and missing data in INLA
#'
#' A wrapper function around "INLA::inla()", providing the necessary structure to fit the hierarchical measurement error model that adjusts coefficient estimates to account for biases due to measurement error and missing data.
#'
#' @param formula_moi an object of class "formula", describing the main model to be fitted.
#' @param formula_imp an object of class "formula", describing the imputation model for the mismeasured and/or missing observations.
#' @param formula_mis an object of class "formula", describing the missingness model. Does not need to have a response variable, since this will always be a binary missingness indicator.
#' @param family_moi a string indicating the likelihood family for the model of interest (the main model).
#' @param data an object of class data.frame or list containing the variables in the model.
#' @param error_type type of error (one or more of "classical", "berkson", "missing")
#' @param error_variable character vector with the name(s) of the variable(s) with error.
#' @param repeated_observations Does the variable with measurement error and/or missingness have repeated observations? If so, set this to "TRUE". In that case, when specifying the formula, use the name of the variable without any numbers, but when specifying the data, make sure that the repeated measurements end in a number, i.e "sbp1" and "sbp2".
#' @param classical_error_scaling can be specified if the classical measurement error varies across observations. Must be a vector of the same length as the data.
#' @param prior.prec.moi a string containing the parameters for the prior for the precision of the residual term for the model of interest.
#' @param prior.prec.berkson a string containing the parameters for the prior for the precision of the error term for the Berkson error model.
#' @param prior.prec.classical a string containing the parameters for the prior for the precision of the error term for the classical error model.
#' @param prior.prec.imp a string containing the parameters for the precision of the latent variable x, which is the variable being described in the imputation model.
#' @param prior.beta.error parameters for the Gaussian prior for the coefficient of the error prone variable.
#' @param prior.gamma.error parameters for the Gaussian prior for the coefficient of the variable with missingness in the missingness model.
#' @param initial.prec.moi the initial value for the precision of the residual term for the model of interest.
#' @param initial.prec.berkson the initial value for the precision of the residual term for the Berkson error term.
#' @param initial.prec.classical the initial value for the precision of the residual term for the classical error term.
#' @param initial.prec.imp the initial value for the precision of the residual term for the latent variable r.
#' @param control.family.moi control.family component for model of interest. Can be specified here using the inla syntax instead of passing the "prior.prec..." and "initial.prec..." arguments, or in the cases when other hyperparameters are needed for the model of interest, see for instance survival models.
#' @param control.family.berkson control.family component Berkson model. Can be specified here using the inla syntax instead of passing the "prior.prec..." and "initial.prec..." arguments. Useful in the cases when more flexibility is needed, for instance if one wants to specify a different prior distribution than Gamma.
#' @param control.family.classical control.family component for classical model. Can be specified here using the inla syntax instead of passing the "prior.prec..." and "initial.prec..." arguments. Useful in the cases when more flexibility is needed, for instance if one wants to specify a different prior distribution than Gamma.
#' @param control.family.imp control.family component for imputation model. Can be specified here using the inla syntax instead of passing the "prior.prec..." and "initial.prec..." arguments. Useful in the cases when more flexibility is needed, for instance if one wants to specify a different prior distribution than Gamma.
#' @param control.family control.family for use in inla (can be provided directly instead of passing the "prior.prec...." and "initial.prec..." arguments. If this is specified, any other "control.family..." or "prior.prec..." arguments provided will be ignored.
#' @param control.predictor control.predictor for use in inla.
#' @param ... other arguments to pass to `inla`.
#'
#' @return An object of class \code{inlamemi}.
#' @export
#'
#' @examplesIf requireNamespace('INLA')
#' # Fit the model
#' simple_model <- fit_inlamemi(data = simple_data,
#'                            formula_moi = y ~ x + z,
#'                            formula_imp = x ~ z,
#'                            family_moi = "gaussian",
#'                            error_type = c("berkson", "classical"),
#'                            error_variable = "x",
#'                            prior.prec.moi = c(10, 9),
#'                            prior.prec.berkson = c(10, 9),
#'                            prior.prec.classical = c(10, 9),
#'                            prior.prec.imp = c(10, 9),
#'                            prior.beta.error = c(0, 1/1000),
#'                            initial.prec.moi = 1,
#'                            initial.prec.berkson = 1,
#'                            initial.prec.classical = 1,
#'                            initial.prec.imp = 1)
fit_inlamemi <- function(formula_moi,
                         formula_imp = NULL,
                         formula_mis = NULL,
                         family_moi,
                         data,
                         error_type = "classical",
                         error_variable = NULL,
                         repeated_observations = FALSE,
                         classical_error_scaling = NULL,
                         prior.prec.moi = NULL,
                         prior.prec.berkson = NULL,
                         prior.prec.classical = NULL,
                         prior.prec.imp = NULL,
                         prior.beta.error = NULL,
                         prior.gamma.error = NULL,
                         initial.prec.moi = NULL,
                         initial.prec.berkson = NULL,
                         initial.prec.classical = NULL,
                         initial.prec.imp = NULL,
                         control.family.moi = NULL,
                         control.family.berkson = NULL,
                         control.family.classical = NULL,
                         control.family.imp = NULL,
                         control.family = NULL,
                         control.predictor = NULL,
                         ...){

  # Some checks ----------------------------------------------------------------
  if(family_moi != "gaussian" && !is.null(prior.prec.moi)){
    warning(paste("The 'prior.prec.moi' argument is only needed when fitting a Gaussian model. You have specified 'family_moi = ",
                  family_moi, "', so 'prior.prec.moi' will not be used."))
  }

  # Extract variables and structure these --------------------------------------
  vars <- extract_variables_from_formula(formula_moi = formula_moi,
                                         formula_imp = formula_imp,
                                         formula_mis = formula_mis,
                                         error_variable = error_variable)

  # Make formula from sub-models -----------------------------------------------
  formula_full <- make_inlamemi_formula(formula_moi = formula_moi,
                               formula_imp = formula_imp,
                               formula_mis = formula_mis,
                               family_moi = family_moi,
                               error_type = error_type,
                               prior.beta.error = prior.beta.error,
                               prior.gamma.error = prior.gamma.error,
                               vars = vars)

  # Make the stacks for the joint model ----------------------------------------
  data_stack <- make_inlamemi_stacks(formula_moi = formula_moi,
                                     formula_imp = formula_imp,
                                     formula_mis = formula_mis,
                                     family_moi = family_moi,
                                     data = data,
                                     error_type = error_type,
                                     repeated_observations = repeated_observations,
                                     vars = vars)

  # Make likelihood vector --------------------------------------------------------

  model_families <- make_inlamemi_families(family_moi = family_moi,
                                           inlamemi_stack = data_stack)

  # Make scaling vector --------------------------------------------------------
  scaling_vec <- make_inlamemi_scaling_vector(
    inlamemi_stack = data_stack,
    error_type = error_type,
    classical_error_scaling = classical_error_scaling,
    vars = vars)

  # Add extra stuff to inla data -----------------------------------------------
  n <- length(data_stack$data$index$moi) # Define number of observations
  data_for_inla <- INLA::inla.stack.data(data_stack,
                                         n = n,
                                         scaling_vec = scaling_vec,
                                         compress = FALSE)


  # Make "control.family" ------------------------------------------------------
  control.family <- make_inlamemi_control.family(
    formula_mis = formula_mis,
    family_moi = family_moi,
    error_type = error_type,
    prior.prec.moi = prior.prec.moi,
    prior.prec.berkson = prior.prec.berkson,
    prior.prec.classical = prior.prec.classical,
    prior.prec.imp = prior.prec.imp,
    initial.prec.moi = initial.prec.moi,
    initial.prec.berkson = initial.prec.berkson,
    initial.prec.classical = initial.prec.classical,
    initial.prec.imp = initial.prec.imp,
    control.family.moi = control.family.moi,
    control.family.berkson = control.family.berkson,
    control.family.classical = control.family.classical,
    control.family.imp = control.family.imp,
    control.family = control.family)

  # Specify "control.predictor" ------------------------------------------------
  # More on control.predictor link for multiple likelihood models here:
  # https://groups.google.com/g/r-inla-discussion-group/c/XZEsk3gR3No
  # Identify which model level is gaussian
  gauss_loc <- which(model_families == "gaussian")[1]
  if(is.null(control.predictor)){
    control.predictor <- list(compute = TRUE, link = gauss_loc)
  }

  # Run everything in the "inla"-function --------------------------------------
  inlamemi_model <- INLA::inla(
    formula = formula_full,
    family = model_families,
    data = data_for_inla,
    scale = scaling_vec,
    control.family = control.family,
    control.predictor = control.predictor,
    ...
  )

  # Set call to be full formula
  inlamemi_model$call <- formula_full

  # Add interesting arguments to output
  inlamemi_model$.args$formula_moi <- formula_moi
  inlamemi_model$.args$formula_imp <- formula_imp
  inlamemi_model$.args$formula_mis <- formula_mis
  inlamemi_model$.args$error_type <- error_type
  inlamemi_model$stack_data <- data_stack

    # Set class ------------------------------------------------------------------
  class(inlamemi_model) <- c("inlamemi", class(inlamemi_model))


  return(inlamemi_model)
}
