test_that("extract_random_effects works", {
  # ONLY random effect
  expected1 <- extract_random_effects(
    formula = y ~ f(id, model = "iid",
                    hyper = list(prec = list(initial = -15, param = c(2, 2))))
  )

  expect_equal(expected1$reff_vars, "id")
  expect_equal(expected1$reff,
               "f(id, model = \"iid\", hyper = list(prec = list(initial = -15, param = c(2, 2))))")

  # Random effect and one covariate
  expected2 <- extract_random_effects(
    formula = y ~ x + f(id, model = "iid",
                        hyper = list(prec = list(initial = -15, param = c(2, 2))))
  )

  expect_equal(expected2$reff_vars, "id")
  expect_equal(expected2$reff,
               "f(id, model = \"iid\", hyper = list(prec = list(initial = -15, param = c(2, 2))))")

})

test_that("extract_variables_from_formula works", {
  # Check that all elements are characters
  expected <- extract_variables_from_formula(formula_moi = y ~ x + z,
                                             formula_imp = x ~ z)
  expect_true(all(sapply(expected, class) %in% c("character", "list")))

  # Check that it correctly identifies interaction between error variable and another covariate.
  expected2 <- extract_variables_from_formula(
    formula_moi = y ~ x + x:z + x:m + z:s,
    formula_imp = x ~ z)
  expect_equal(expected2[["error_interaction_list"]],
               list(c("x", "z"), c("x", "m")))

  # Make sure it also works if only term is interaction term
  expected3 <- extract_variables_from_formula(formula_moi = y ~ smoking:sbp,
                                              formula_imp = sbp ~ smoking)
  expect_equal(expected3[["error_interaction_list"]], list(c("smoking", "sbp")))

  # Multiple variables with error
  expected4 <- extract_variables_from_formula(formula_moi = y ~ x + v + z + s,
                                              formula_imp = c(x ~ z, v ~ z + s),
                                              error_variable = c("x", "v"))
  expect_equal(length(expected4$covariates_imp), 2)

  # Random effects in MOI and imputation model
  # ONLY random effect
  expected5 <- extract_variables_from_formula(
    formula_moi = y ~ f(id, model = "iid", hyper = list(prec = list(initial = -15, param = c(2, 2)))),
    formula_imp = x ~ f(group, model = "iid", hyper = list(prec = list(initial = -15, param = c(2, 2)))))

  expect_equal(expected5$random_effect_variables_imp[[1]], "group")
  expect_equal(expected5$random_effect_variables_moi[[1]], "id")

  # Random effect and one covariate
  expected6 <- extract_variables_from_formula(
    formula_moi = y ~ x + f(id, model = "iid", hyper = list(prec = list(initial = -15, param = c(2, 2)))),
    formula_imp = x ~ s + f(group, model = "iid", hyper = list(prec = list(initial = -15, param = c(2, 2)))))

  expect_equal(expected6$random_effect_variables_imp[[1]], "group")
  expect_equal(expected6$covariates_imp[[1]], "s")

  # Multiple error variables
  expected7 <- extract_variables_from_formula(formula_moi = y ~ x1 + x2:z,
                                              formula_imp = list(x1 ~ z, x2 ~ 1))

  expect_equal(expected7$covariates_imp, list("z", character(0)))

  # Missingness model with random effect
  expected8 <- extract_variables_from_formula(
    formula_moi = y ~ x + z1 + z2,
    formula_imp = x ~ z1,
    formula_mis = m ~ z2 + x + f(id, model = "iid", hyper = list(prec = list(initial = -15, param = c(2, 2)))))

  expect_equal(expected8$random_effect_variables_mis[[1]], "id")
  expect_equal(expected8$covariates_mis[[1]], c("z2", "x"))

  # Multiple error variables with missingness models
  expected9 <- extract_variables_from_formula(formula_moi = y ~ x1 + x2 + z1 + z2 + z3,
                                              formula_imp = list(x1 ~ z1, x2 ~ 1),
                                              formula_mis = list(m1 ~ z2 + z3, m2 ~ z1))

  expect_equal(expected9$covariates_mis, list(c("z2", "z3"), "z1"))

  # Catching errors -----------------------------------------------------------
  expect_error(extract_variables_from_formula(formula_moi = "string", formula_imp = x ~ z))
  expect_error(extract_variables_from_formula(formula_moi = y ~ x + z,
                                              formula_imp = y ~ z,
                                              error_variable = "x"))

})

test_that("make_inlamemi_formula works", {
  skip_on_cran()

  # Multiple error variables
  formula1 <- make_inlamemi_formula(formula_moi = y ~ x1 + x2 + z,
                      formula_imp = list(x1 ~ z, x2 ~ 1),
                      prior.beta.error = c(0, 1/1000),
                      error_variable = c("x1", "x2"))
  terms1 <- labels(terms(formula1))
  expect_in(c("alpha.x1.0", "alpha.x1.z", "alpha.x2.0"), terms1)
  expect_equal(length(terms1), 9)

  # Multiple error variables and interaction effect
  formula2 <- make_inlamemi_formula(formula_moi = y ~ x1 + x2:z,
                                  formula_imp = list(x1 ~ z, x2 ~ 1),
                                  prior.beta.error = c(0, 1/1000),
                                  error_variable = c("x1", "x2"))
  terms2 <- labels(terms(formula2))
  expect_equal(length(terms2), 10)

  # Basic case
  formula3 <- make_inlamemi_formula(formula_moi = y ~ x + z,
                                  formula_imp = x ~ z,
                                  prior.beta.error = c(0, 1/1000))
  terms3 <- labels(terms(formula3))
  expect_equal(length(terms3), 6)

  # Missingness model
  formula4 <- make_inlamemi_formula(formula_moi = y ~ x + z,
                        formula_imp = x ~ z,
                        formula_mis = m ~ z,
                        prior.beta.error = c(0, 1/1000),
                        prior.gamma.error = c(0, 1/1000))
  terms4 <- labels(terms(formula4))
  expect_equal(length(terms4), 9)

  # Simple linear regression
  formula_slr <- make_inlamemi_formula(formula_moi = y ~ x,
                                       formula_imp = x ~ 1,
                                       prior.beta.error = c(0, 1/1000))
  terms_slr <- labels(terms(formula_slr))
  expect_equal(length(terms_slr), 4)
})

test_that("make_inlamemi_stacks works", {
  skip_on_cran()

  # Stacks ----
  # Simulated data, stacks with berkson level
  simple_stacks_cb <- make_inlamemi_stacks(formula_moi = y ~ x + z,
                                           formula_imp = x ~ z,
                                           data = simple_data,
                                           error_type = c("classical", "berkson"))

  # Check rows and columns of "data"
  expect_equal(nrow(simple_stacks_cb$data$data), 4*1000)
  expect_equal(ncol(simple_stacks_cb$data$data), 4)

  # Check rows and columns of "effects"
  expect_equal(nrow(simple_stacks_cb$effects$data), 4*1000)
  expect_equal(ncol(simple_stacks_cb$effects$data), 9)

  # Check the names
  expected_names_effects <- c("beta.0", "id.x", "weight.x", "id.r", "weight.r",
                      "alpha.x.0", "beta.x", "beta.z", "alpha.x.z")
  actual_names <- unlist(simple_stacks_cb$effects$names)
  names(actual_names) <- NULL
  expect_equal(sort(actual_names), sort(expected_names_effects))

  # Framingham data
  framingham_stacks <- make_inlamemi_stacks(data = framingham,
                                          formula_moi = disease ~ sbp + smoking,
                                          formula_imp = sbp ~ smoking,
                                          error_type = "classical",
                                          repeated_observations = TRUE)
  # Check rows and columns of "data"
  expect_equal(nrow(framingham_stacks$data$data), 4*641)
  expect_equal(ncol(framingham_stacks$data$data), 3)

  # Check rows and columns of "effects"
  expect_equal(nrow(framingham_stacks$effects$data), 4*641)
  expect_equal(ncol(framingham_stacks$effects$data), 7)

  # Multiple error variables
  mult_error_stack <- make_inlamemi_stacks(data = two_error_data,
                                         formula_moi = y ~ x1 + x2 + z,
                                         formula_imp = list(x1 ~ z, x2 ~ 1),
                                         error_type = list("classical", "classical"))

  expect_equal(length(mult_error_stack$effects$names), 11)

  # Should check random effect example here

  # Missingness model
  missingness_model_stack <- make_inlamemi_stacks(data = simple_data,
                                                  formula_moi = y ~ x + z,
                                                  formula_imp = x ~ z,
                                                  formula_mis = m ~ z + x,
                                                  error_type = "missing")

  # Simple linear regression
  # Simple linear regression
  stacks_slr <- make_inlamemi_stacks(formula_moi = y ~ x,
                                     formula_imp = x ~ 1,
                                     data = simple_data)


  # Catching errors -----------------------------------------------------------

  # Check if the error variable is called "error_variable"
  formula_moi <- mpg ~ error_variable + cyl + disp
  formula_imp <- error_variable ~ cyl
  expect_error(make_inlamemi_stacks(data = mtcars,
                             formula_moi = formula_moi,
                             formula_imp = formula_imp))

  # Repeated observation stuff --
  # Check that the error variable is actually in the data
  expect_error(make_inlamemi_stacks(data = framingham,
                                 formula_moi = disease ~ SBP + smoking,
                                 formula_imp = SBP ~ smoking,
                                 error_type = "classical"))
  # Check that repeated_observations has been set to TRUE
  framingham$sbp <- rep(NA, nrow(framingham)) # (normally the column will not be in the data, if i don't do this we get an errro and a warning message, and idk how to expect that)
  expect_warning(make_inlamemi_stacks(data = framingham,
                                    formula_moi = disease ~ sbp + smoking,
                                    formula_imp = sbp ~ smoking,
                                    error_type = "classical"))

  # Check if variables in formula are in data (but not sensitive to repeated measurements)
  expect_error(make_inlamemi_stacks(
    formula_moi = inla.surv(t, d) ~ sbp + aged + smoking + sex + diabetes,
    formula_imp = sbp ~ age + smoke + sex + diabetes,
    data = nhanes_survival,
    error_type = c("classical", "missing"),
    repeated_observations = TRUE)
  )

})

test_that("make_inlamemi_families works", {
  skip_on_cran()

  #Testing survival model and missingness model
  surv_stack <- make_inlamemi_stacks(
    formula_moi = inla.surv(t, d) ~ sbp + age + smoke + sex + diabetes,
    formula_imp = sbp ~ age + smoke + sex + diabetes,
    formula_mis = m ~ smoke,
    family_moi = "weibull.surv",
    data = nhanes_survival,
    error_type = c("classical", "missing"),
    repeated_observations = TRUE)

  families_surv <- make_inlamemi_families(family_moi = "weibull.surv",
                                          inlamemi_stack = surv_stack)
  expect_equal(families_surv, c("weibull.surv", "gaussian", "gaussian", "binomial"))

  nhanes_stack <- make_inlamemi_stacks(
    formula_moi = inla.surv(t, d) ~ sbp + age + smoke + sex + diabetes,
    formula_imp = sbp ~ age + smoke + sex + diabetes,
    data = nhanes_survival,
    error_type = c("classical", "missing"),
    repeated_observations = TRUE)

})

test_that("make_inlamemi_scaling_vector works", {
  # Check that the scaling vector is the correct length
  expect_error(
    make_inlamemi_scaling_vector(simple_data,
                               error_variable = "x",
                               error_type = c("classical", "berkson"),
                               classical_error_scaling = rep(1, 500)))
})

test_that("make_inlamemi_control.family works", {
  skip_on_cran()

  cont.fam <- make_inlamemi_control.family(
    family_moi = "gaussian",
    error_type = c("berkson", "classical"),
    prior.prec.moi = c(10, 9),
    prior.prec.berkson = c(10, 9),
    prior.prec.classical = c(10, 9),
    prior.prec.imp = c(10, 9),
    initial.prec.moi = 1,
    initial.prec.berkson = 1,
    initial.prec.classical = 1,
    initial.prec.imp = 1)

  expect_equal(length(cont.fam), 4)
  expect_equal(cont.fam[[1]]$hyper$prec$initial, log(1))
  expect_equal(cont.fam[[1]]$hyper$prec$param, c(10, 9))

  # With missingness model
  cont.fam_mis <- make_inlamemi_control.family(
    formula_mis = m ~ x + d,
    family_moi = "gaussian",
    error_type = c("berkson", "classical"),
    prior.prec.moi = c(10, 9),
    prior.prec.berkson = c(10, 9),
    prior.prec.classical = c(10, 9),
    prior.prec.imp = c(10, 9),
    initial.prec.moi = 1,
    initial.prec.berkson = 1,
    initial.prec.classical = 1,
    initial.prec.imp = 1)

  expect_equal(length(cont.fam_mis), 5)

})

test_that("fit_inlamemi works", {
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

  # Test if the number of hyperparameters is 5
  expect_equal(nrow(simple_model$summary.hyperpar), 5)

  # Check that the model hass the correct class
  expect_s3_class(simple_model, c("inlamemi", "inla"))

  # Check the estimate for beta.x and the precision of MOI-level
  beta.x <- simple_model$summary.hyperpar["Beta for beta.x", "mean"]
  expect_equal(beta.x, 2, tolerance = 0.3)
  prec.moi <- simple_model$summary.hyperpar["Precision for the Gaussian observations", "mean"]
  expect_equal(prec.moi, 1, tolerance = 0.2)

  # Multiple error variables
  prior.prec.y <- prior.prec.x1.u_c <-  prior.prec.x1 <- prior.prec.x2.u_c <- prior.prec.x2 <- c(10, 9)
  initial.prec.y <- initial.prec.x1.u_c <- initial.prec.x1 <- initial.prec.x2.u_c <- initial.prec.x2 <- 1

  control.family <- list(
    list(hyper = list(prec = list(initial = log(initial.prec.y),
                                  param = prior.prec.y,
                                  fixed = FALSE))),
    list(hyper = list(prec = list(initial = log(initial.prec.x1.u_c),
                                  param = prior.prec.x1.u_c,
                                  fixed = FALSE))),
    list(hyper = list(prec = list(initial = log(initial.prec.x1),
                                  param = prior.prec.x1,
                                  fixed = FALSE))),
    list(hyper = list(prec = list(initial = log(initial.prec.x2.u_c),
                                  param = prior.prec.x2.u_c,
                                  fixed = FALSE))),
    list(hyper = list(prec = list(initial = log(initial.prec.x2),
                                  param = prior.prec.x2,
                                  fixed = FALSE)))
  )

  # Multiple error variables
  multiple_model <- fit_inlamemi(formula_moi = y ~ x1 + x2 + z,
                               formula_imp = list(x1 ~ z, x2 ~ 1),
                               family_moi = "gaussian",
                               data = two_error_data,
                               error_type = list("classical", "classical"),
                               prior.beta.error = c(0, 1/1000),
                               control.family = control.family,
                               control.predictor = list(compute = TRUE)
  )

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

  #class(mis_mod) <- "inla"
  summary(mis_mod)

  labels(terms(mis_mod$.args$formula))

  # Simple linear regression
  inlamemi_model <- fit_inlamemi(formula_moi = y ~ x,
                                 formula_imp = x ~ 1,
                                 family_moi = "gaussian",
                                 data = simple_data,
                                 prior.prec.moi = c(10, 9),
                                 prior.prec.classical = c(0.5, 0.5),
                                 prior.prec.imp = c(0.5, 0.5),
                                 initial.prec.moi = 1,
                                 initial.prec.classical = 1/2,
                                 initial.prec.imp = 1/1.5)

  # Catching errors -----------------------------------------------------------

})
