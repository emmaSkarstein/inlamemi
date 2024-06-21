
#' Plot model summary
#'
#' @param x the model returned from the fit_inlamemi function.
#' @param plot_moi should the posterior mean for the coefficients of the model of interest be plotted? Defaults to TRUE.
#' @param plot_imp should the posterior mean for the coefficients of the imputation model be plotted? Defaults to TRUE.
#' @param plot_mis should the posterior mean for the coefficients of the missingness model be plotted? Defaults to TRUE.
#' @param plot_intercepts should the posterior mean for the intercept(s) be plotted? Defaults to TRUE.
#' @param error_variable_highlight should the coefficient(s) of the variable(s) with error be highlighted? (circled in black) Defaults to FALSE.
#' @param greek make the coefficient names into greek letters with the covariate name as subscript. Defaults to FALSE.
#' @param palette either a number (between 1 and 5), indicating the number of the color palette to be used, or a vector of the colors to be used.
#' @param ... other arguments
#'
#' @return An object of class "ggplot2" that plots the posterior mean and 95 \% credible interval for each coefficient in the model. The coefficients are colored to indicate if they belong to the main or imputation model, and the variable with error is also highlighted.
#' @method plot inlamemi
#' @export
#'
#' @importFrom ggplot2 ggplot aes vars
#' @importFrom rlang .data
#' @importFrom scales parse_format
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
#' plot(simple_model)
plot.inlamemi <- function(x,
                        plot_moi = TRUE,
                        plot_imp = TRUE,
                        plot_mis = TRUE,
                        plot_intercepts = TRUE,
                        error_variable_highlight = FALSE,
                        greek = FALSE,
                        palette = NULL,
                        ...){
  # Plot the posterior mean and 0.975 and 0.025 quantiles for all the coefficients
  # Highlight the error prone variable
  # Return ggplot2 object so that it can be further modified by user

  # What other arguments would be useful?
  # - Which covariates to plot
  # - Font? (check if this can be changed for the ggplot object after being returned)
  # - colors? (check if this can be changed for the ggplot object after being returned)

  # I think a useful principle can be to keep the stylistic stuff as basic as
  # possible, as long as it is possible for the user to change once the object
  # is returned.

  simple_summary <- simplify_inlamemi_model_summary(x)


  # joint_summary_df <- dplyr::bind_rows(moi_coef = simple_summary$moi_coef[,1:6],
  #                                      error_coef = simple_summary$error_coef[,1:6],
  #                                      imp_coef = simple_summary$imp_coef[,1:6],
  #                                      mis_coef = simple_summary$mis_coef[,1:6],
  #                                      .id = "coefficient_type") |>
  #   dplyr::rename(quant_0.025 = "0.025quant", quant_0.975 = "0.975quant") |>
  #   dplyr::mutate(coefficient_type = dplyr::recode(.data$coefficient_type,
  #                                                  error_coef = "Variable w. error",
  #                                                  imp_coef = "Imputation model",
  #                                                  moi_coef = "Model of interest",
  #                                                  mis_coef = "Missingness model"))

  joint_summary_df <- dplyr::bind_rows(moi_coef = simple_summary$moi_coef[,1:6],
                                       error_coef = simple_summary$error_coef[,1:6],
                                       imp_coef = simple_summary$imp_coef[,1:6],
                                       mis_coef = simple_summary$mis_coef[,1:6],
                                       .id = "coefficient_type")
  joint_summary_df$variable_raw <- rownames(joint_summary_df)
  joint_summary_df$model_type[grepl("beta", joint_summary_df$variable_raw)] <- "moi"
  joint_summary_df$model_type[grepl("alpha", joint_summary_df$variable_raw)] <- "imp"
  joint_summary_df$model_type[grepl("gamma", joint_summary_df$variable_raw)] <- "mis"

  joint_summary_df <- joint_summary_df |>
    dplyr::rename(quant_0.025 = "0.025quant", quant_0.975 = "0.975quant") |>
    dplyr::mutate(model_type = dplyr::recode(.data$model_type,
                                                   imp = "Imputation model",
                                                   moi = "Model of interest",
                                                   mis = "Missingness model"))

  joint_summary_df$error_indicator <- ifelse(
    joint_summary_df$coefficient_type == "error_coef", 1, 0)


  if(!plot_moi && !plot_imp){
    stop("This will plot nothing. Please set either 'plot_moi' or 'plot_imp' to TRUE.")
  }

  if(!plot_moi){
    # If plot_moi == FALSE, only plot the imputation model coefficients.
    joint_summary_df <- dplyr::filter(joint_summary_df,
                                      .data$model_type != "Missingness model")
  }
  if(!plot_imp){
    # If plot_imp == FALSE, do not plot the imputation model coefficients.
    joint_summary_df <- dplyr::filter(joint_summary_df,
                                      .data$model_type != "Imputation model")
  }
  if(!plot_mis){
    joint_summary_df <- dplyr::filter(joint_summary_df,
                                      .data$model_type != "Missingness model")
  }
  if(!plot_intercepts){
    joint_summary_df <- dplyr::filter(joint_summary_df,
                                      !endsWith(.data$variable_raw, ".0"))
  }

  joint_summary_df$model_type <- factor(joint_summary_df$model_type,
                                        levels = c("Model of interest",
                                                   "Imputation model",
                                                   "Missingness model"))

  # Greek letter coefficients
  joint_summary_df$var1 <- sub("\\..*", "", joint_summary_df$variable_raw)
  joint_summary_df$var2 <- sub(".*\\.", "", joint_summary_df$variable_raw)
  joint_summary_df$variable_greek <- paste0(joint_summary_df$var1,
                                                        "[", joint_summary_df$var2, "]")

  if(greek){
    joint_summary_df$variable <- joint_summary_df$variable_greek
  }else{
    joint_summary_df$variable <- joint_summary_df$variable_raw
  }

  # Correct order of coefficients
  variable_levels <- joint_summary_df |>
    dplyr::arrange(.data$model_type) |>
    dplyr::select(.data$variable)

  joint_summary_df$variable <- factor(joint_summary_df$variable,
                                      levels = variable_levels[,1])


  # Set up the color palette
  color_pal_list <- list()
  color_pal_list[[2]] <- c("#7AA6CA", "#DDAA33", "#EE849D")

  color_pal_list[[3]] <- c("#347789", "#E4B15B", "#C2535E")

  color_pal_list[[1]] <- c("#fb8b24", "#d90368", "#820263")

  color_pal_list[[4]] <- c("#c9cba3", "#ffe1a8", "#e26d5c")

  color_pal_list[[5]] <- c("#605E8F", "#799EA2", "#C15B41")

  if(!is.null(palette)){
    if(is.numeric(palette)){
      color_pal <- color_pal_list[[palette]]
    }else{
      color_pal <- palette
    }
  }else{
    color_pal <- color_pal_list[[1]]
  }

  coef_plot <- ggplot(joint_summary_df,
                      aes(y = .data$variable)) +
    # Highlight error coef
    {if(error_variable_highlight)
      ggplot2::geom_point(aes(x = .data$mean,
                              fill = NULL,
                              alpha = .data$error_indicator),
                          size = 4, shape = 21)} +
    # Error lines
    ggplot2::geom_linerange(aes(xmin = .data$quant_0.025,
                                xmax = .data$quant_0.975,
                                color = .data$model_type),
                            linewidth = 1) +
    # Point for mean
    ggplot2::geom_point(aes(x = .data$mean,
                            color = .data$model_type),
                        size = 3) +
    # Numeric text at mean
    ggplot2::geom_text(aes(x = .data$mean,
                           y = .data$variable,
                           label = round(.data$mean, digits = 3)),
                       vjust = -1, size = 3.5) +
    # Scale y-axis to avoid cutting off text ( think defaults are mult = .05, add = .6)
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.05, 0.1),
                                                          add = 0.6),
                              limits = rev,
                              labels = scales::parse_format()) +
    # Colors
    ggplot2::scale_color_manual(values = color_pal) +
    # Transparency scale
    ggplot2::scale_alpha(range = c(0, 1), guide = "none") +
    # Lables
    ggplot2::labs(x = "Posterior mean and 95% credible intervals",
                  y = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "bottom")

  coef_plot
}
