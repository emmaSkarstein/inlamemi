#' Simplify the "raw" model summary for printing and plotting
#'
#' @param inlamemi_model the model returned from the fit_inlamemi function.
#'
#' @return A list of four data frames, containing the summaries for different components of the model. These are the coefficients of the model of interest, the coefficient of the variable with error, the coefficients of the imputation model, and the hyperparameters.
#' @keywords internal
simplify_inlamemi_model_summary <- function(inlamemi_model){
  # Extract the fixed effects and the hyperparameters from the inla summary
  fixed <- inlamemi_model$summary.fixed
  hyper <- inlamemi_model$summary.hyperpar

  # Identify the moi coefs and the imputation coefs, based on whether they start with alpha or beta.
  moi_coef <- dplyr::filter(fixed, grepl("beta.", rownames(fixed)))[,1:6]
  imp_coef <- dplyr::filter(fixed, grepl("alpha.", rownames(fixed)))[,1:6]
  mis_coef <- dplyr::filter(fixed, grepl("gamma.", rownames(fixed)))[,1:6]

  error_coef <- dplyr::filter(hyper, grepl("Beta for ", rownames(hyper)))

  error_var_name <- sub(".*Beta for ", "", rownames(error_coef))
  rownames(error_coef) <- error_var_name

  other_model_hyperpar <- dplyr::filter(hyper, !grepl("Beta for ", rownames(hyper)))

  # Set better names for the hyperparameter precisions
  precision_moi <- inlamemi_model$.args$family[1] == "gaussian"
  prec_names <- c()
  if(precision_moi){
    prec_names <- c("Precision for model of interest")
  }
  all_responses <- as.list(inlamemi_model$.args$formula[[2]])
  non_moi <- all_responses[3:length(all_responses)]
  model_names <- sub(".*_", "", non_moi)

  error_and_imp <- non_moi[model_names %in% c("classical", "berkson", "imp")]

  prec_names_error_imp <- sapply(error_and_imp,
                                 function(x){paste("Precision for",
                                                   gsub("_", " ", toString(x)),
                                                   "model")})
  prec_names <- c(prec_names, prec_names_error_imp)
  which_prec_for_model_levels <- grepl("Precision for the Gaussian observations",
                                       rownames(other_model_hyperpar))
  other_prec <- other_model_hyperpar[!which_prec_for_model_levels, ]
  if(nrow(other_prec)>0){
    rownames(other_model_hyperpar) <- c(prec_names, rownames(other_prec))
  }else{
    rownames(other_model_hyperpar) <- prec_names
  }


  return(list(moi_coef = moi_coef,
              error_coef = error_coef,
              imp_coef = imp_coef,
              mis_coef = mis_coef,
              other_model_hyperpar = other_model_hyperpar))
}

#' Summary method for inlamemi
#'
#' Takes a fitted `inlamemi` object produced by
#' `fit_inlamemi` and produces a summary from it.
#'
#' @aliases summary.inlamemi print.summary.inlamemi
#' @param object model of class `inlamemi`.
#' @param ... other arguments
#'
#' @return `summary.inlamemi` returns an object of class `summary.inlamemi`, a list of components to print.
#' @method summary inlamemi
#' @rdname summary
#' @export
#'
#' @examplesIf requireNamespace('INLA')
#' # Fit the model
#' simple_model <- fit_inlamemi(data = simple_data,
#'                            formula_moi = y ~ x + z,
#'                            formula_imp = x ~ z,
#'                            family_moi = "gaussian",
#'                            error_type = c("berkson", "classical"),
#'                            prior.prec.moi = c(10, 9),
#'                            prior.prec.berkson = c(10, 9),
#'                            prior.prec.classical = c(10, 9),
#'                            prior.prec.imp = c(10, 9),
#'                            prior.beta.error = c(0, 1/1000),
#'                            initial.prec.moi = 1,
#'                            initial.prec.berkson = 1,
#'                            initial.prec.classical = 1,
#'                            initial.prec.imp = 1)
#'
#' summary(simple_model)
summary.inlamemi <- function(object, ...){
  inlamemi_summary <- simplify_inlamemi_model_summary(object)

  inlamemi_summary$formula_moi <- object$.args$formula_moi
  inlamemi_summary$formula_imp <- object$.args$formula_imp
  inlamemi_summary$formula_mis <- object$.args$formula_mis
  inlamemi_summary$error_type <- object$.args$error_type

  class(inlamemi_summary) <- "summary.inlamemi"

  return(inlamemi_summary)
}

#' Print method for summary.inlamemi
#'
#' @param x object of class summary.inlamemi.
#' @param ... other arguments
#'
#' @method print summary.inlamemi
#' @rdname summary
#' @export
#'
print.summary.inlamemi <- function(x, ...){
  cat("Formula for model of interest: \n")
  print(x$formula_moi, showEnv = FALSE)
  cat("\n")

  cat("Formula for imputation model: \n")
  print(x$formula_imp, showEnv = FALSE)
  cat("\n")

  if(!is.null(x$formula_mis)){
    cat("Formula for missingness model: \n")
    print(x$formula_mis, showEnv = FALSE)
    cat("\n")
  }

  cat("Error types: \n")
  print(x$error_type)
  cat("\n")

  cat("Fixed effects for model of interest: \n")
  print(x$moi_coef)
  cat("\n")

  cat("Coefficient for variable with measurement error and/or missingness: \n")
  print(x$error_coef)
  cat("\n")

  cat("Fixed effects for imputation model: \n")
  print(x$imp_coef)
  cat("\n")

  if(!is.null(x$formula_mis)){
    cat("Fixed effects for missingness model: \n")
    print(x$mis_coef)
    cat("\n")
  }

  error_string <- paste(rownames(x$error_coef), collapse = ", ")
  cat(paste0("Model hyperparameters (apart from ",
             error_string, "): \n"))
  print(x$other_model_hyperpar)
  cat("\n")
}

#' Print method for inlamemi
#'
#' @param x object of class `inlamemi`.
#' @param ... other arguments.
#'
#' @method print inlamemi
#' @rdname print
#' @export
#'
print.inlamemi <- function(x, ...){
  cat("Formula for model of interest: \n")
  print(x$.args$formula_moi, showEnv = FALSE)
  cat("\n")

  cat("Formula for imputation model: \n")
  print(x$.args$formula_imp, showEnv = FALSE)
  cat("\n")

  if(!is.null(x$.args$formula_mis)){
    cat("Formula for missingness model: \n")
    print(x$.args$formula_mis, showEnv = FALSE)
    cat("\n")
  }

  cat("Error types: \n")
  print(x$.args$error_type)
  cat("\n")

  print(coef.inlamemi(x), ...)
}

#' Visualize the model data structure as matrices in LaTeX
#'
#' @param stack an object of class inla.stack returned from the function make_inlamemi_stacks, which describes the structure of the data for the measurement error and imputation model.
#'
#' @return A list containing data frames with the left hand side (response_df) and right hand side (effects_df), along with the latex code needed to visualize the matrices (matrix_string).
#' @export
#'
#' @examplesIf requireNamespace('INLA')
#' stack <- make_inlamemi_stacks(data = simple_data,
#'                    formula_moi = y ~ x + z,
#'                    formula_imp = x ~ z,
#'                    error_type = "classical")
#' show_data_structure(stack)
show_data_structure <- function(stack){
  response_df <- round(stack$data$data, 2)
  effects_df <- round(stack$effects$data, 2)

  # Multiply weights with ids
  effects_mult_df <- effects_df
  id.vars <- colnames(effects_mult_df)[grepl("id.", colnames(effects_mult_df), fixed = TRUE)]

  for(variable in id.vars){
    # Check if it is actually an id variable, and not just a variable that happens to contain "id."
    # I check this by looking for a "weight" variable with the same suffix
    weight <- paste0("weight.", sub(".*\\.", "", variable))
    if(weight %in% colnames(effects_mult_df)){
      mult_id <- effects_df[variable]*effects_df[weight]
      effects_mult_df[variable] <- mult_id
    }
  }

  response <- list()
  effects <- list()

  for(model in names(stack$data$index)){
    index <- stack$data$index[model][[1]]

    # Building response matrix ----
    sub_response <- response_df[index, ]
    sub_response_top <- sub_response[1, ]
    sub_response_bottom <- sub_response[nrow(sub_response), ]

    vdot_response <- rep("\\vdots", ncol(response_df))

    response <- rbind(response, sub_response_top, vdot_response, sub_response_bottom)

    # Building effects matrix ----
    sub_effects <- effects_mult_df[index, ]
    sub_effects_top <- sub_effects[1, ]
    sub_effects_bottom <- sub_effects[nrow(sub_effects), ]

    vdot_effects <- rep("\\vdots", ncol(effects_mult_df))

    effects <- rbind(effects, sub_effects_top, vdot_effects, sub_effects_bottom)
  }

  # Constructing response bmatrix
  response_table <- knitr::kable(response, format = "latex",
                                 row.names = FALSE, escape = FALSE,
                                 booktabs = TRUE, linesep = "")
  response_matrix_body <- gsub('^.*\\\\midrule\\s*|\\s*\\\\bottomrule.*$', '', response_table)
  response_matrix <- paste0("\\begin{bmatrix} \n", response_matrix_body, "\n\\end{bmatrix}")
  underbrace_response <- paste0("\\underbrace{", response_matrix, "}_{\\texttt{Y}}")



  # List of all column names in effects except the weights
  colnames_except_weight <- colnames(effects)[grep("weight", colnames(effects),
                                                   invert = TRUE)]
  # Constructing effects bmatrix vectors
  effect_matrices <- list()

  for(variable in colnames_except_weight){
    if(grepl("id.", variable, fixed = TRUE)){
      coef_name <- ""
    }else{
      var_comps <- unlist(strsplit(variable, split = "[.]"))
      var_coef <- var_comps[1]
      var_1 <- var_comps[2]
      if(length(var_comps) == 3){
        var_2 <- var_comps[3]
        var_2_string <- paste0(",", var_2)
      }else{
        var_2_string <- ""
      }
      coef_name <- paste0("\\", var_coef, "_{", var_1, var_2_string, "}")
    }
    effect_table <- knitr::kable(effects[variable], format = "latex",
                                 row.names = FALSE, escape = FALSE,
                                 booktabs = TRUE, linesep = "")
    effect_matrix_body <- gsub('^.*\\\\midrule\\s*|\\s*\\\\bottomrule.*$', '', effect_table)
    effect_matrix <- paste0("\\begin{bmatrix} \n", effect_matrix_body, "\n\\end{bmatrix}")

    underbrace_effects <- paste0(coef_name, "\\underbrace{", effect_matrix, "}_{\\texttt{", variable, "}}")

    effect_matrices[[variable]] <- underbrace_effects
  }

  # Put it all together
  all_effect_matrices <- paste0(effect_matrices, collapse = " + ")
  all_of_it <- paste0("$$", underbrace_response, "\n = \n", all_effect_matrices, "$$")

  all_of_it_verb <- gsub("NA", "\\\\texttt{NA}", all_of_it)

  # message("Make sure to include '\\usepackage{amsmath}', as this is required to display the matrices correctly.")

  return(list(response_df = response,
              effects_df = effects,
              matrix_string = all_of_it_verb))
}
