test_that("plot_inlamemi works", {
  simple_moi <- y ~ x + z
  simple_imp <- x ~ z

  # Fit the model
  simple_model <- fit_inlamemi(data = simple_data,
                             formula_moi = simple_moi,
                             formula_imp = simple_imp,
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

  # Catching errors -----------------------------------------------------------
  # Error when user specifies no moi and no imp coefs
  expect_error(plot_inlamemi(simple_model, plot_moi = FALSE, plot_imp = FALSE))
})
