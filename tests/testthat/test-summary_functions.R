test_that("summary.inlamemi works", {
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

  inlamemi_summary <- summary(simple_model)

  # Check summary class
  expect_s3_class(inlamemi_summary, "summary.inlamemi")

  # Catching errors -----------------------------------------------------------

})
