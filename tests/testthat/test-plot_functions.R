test_that("plot_inlamemi works", {
  skip_on_cran()

  # Fit the model
  simple_model <- fit_inlamemi(data = simple_data,
                             formula_moi = y ~ x + z,
                             formula_imp = x ~ z,
                             family_moi = "gaussian",
                             error_type = c("berkson", "classical"),
                             prior.prec.moi = c(10, 9),
                             prior.prec.berkson = c(10, 9),
                             prior.prec.classical = c(10, 9),
                             prior.prec.imp = c(10, 9),
                             prior.beta.error = c(0, 1/1000),
                             initial.prec.moi = 1,
                             initial.prec.berkson = 1,
                             initial.prec.classical = 1,
                             initial.prec.imp = 1)
  plot(simple_model, greek_coefficients = TRUE)

  # Missingness model
  mis_mod <- fit_inlamemi(formula_moi = y ~ x + z1 + z2,
                          formula_imp = x ~ z1,
                          formula_mis = m ~ z2 + x,
                          family_moi = "gaussian",
                          data = mar_data,
                          error_type = "missing",
                          prior.beta.error = c(0, 1/1000),
                          prior.gamma.error = c(0, 1/1000),
                          prior.prec.moi = c(10, 9),
                          prior.prec.imp = c(10, 9),
                          initial.prec.moi = 1,
                          initial.prec.imp = 1)


  plot(mis_mod, error_variable_highlight = FALSE)# +
    ggplot2::facet_wrap(~model_type, scales = "free_x")

  plot_obj <- plot(mis_mod)

  # Catching errors -----------------------------------------------------------
  # Error when user specifies no moi and no imp coefs
  expect_error(plot_inlamemi(simple_model, plot_moi = FALSE, plot_imp = FALSE))
})
