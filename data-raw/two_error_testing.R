# Testing multiple covariates with error
library(INLA)

attach(two_error_data)
n <- nrow(two_error_data)

# Modelling ----
# Truth:
# alpha.x1.0 <- 1; alpha.x1.z <- 2
# alpha.x2.0 <- 1
# beta.0 <- 1; beta.x1 <- 2; beta.x2 <- 2; beta.z <- 2
# all precisions/sd are 1

# Regression model of interest
# y = beta.0 + beta.x1*x1 + beta.x2*x2 + beta.z*z + e
stk_moi <- inla.stack(data = list(y_moi = y),
                      A = list(1),
                      effects = list(
                        list(beta.0 = rep(1, n),
                             beta.x1 = 1:n,
                             beta.x2 = 1:n,
                             beta.z = z)),
                      tag = "moi")

# Classical measurement error model x1
# x1 = w1 + u_c
stk_x1_c <- inla.stack(data = list(y_x1_classical = x1),
                    A = list(1),
                    effects = list(
                      list(id.x1 = 1:n,
                           weight.x1 = 1)),
                    tag = "classical_x1")

# Classical measurement error model x2
# x2 = w2 + u_x2_c
stk_x2_c <- inla.stack(data = list(y_x2_classical = x2),
                       A = list(1),
                       effects = list(
                         list(id.x2 = 1:n,
                              weight.x2 = 1)),
                       tag = "classical_x2")

# Imputation/exposure model x1
# 0 = x1 + alpha.x1.0 + alpha.x1.z + e_x1
stk_x1_imp <- inla.stack(data = list(y_x1_imp = rep(0, n)),
                      A = list(1),
                      effects = list(
                        list(id.x1 = 1:n,
                             weight.x1 = rep(-1, n),
                             alpha.x1.0 = rep(1, n),
                             alpha.x1.z = z)),
                      tag = "imputation_x1")


# Imputation/exposure model x2
# 0 = x2 + alpha.x2.0 + e_x2
stk_x2_imp <- inla.stack(data = list(y_x2_imp = rep(0, n)),
                      A = list(1),
                      effects = list(
                        list(id.x2 = 1:n,
                             weight.x2 = rep(-1, n),
                             alpha.x2.0 = rep(1, n))),
                      tag = "imputation_x2")

# Stack them on top of each other
stk_full <- inla.stack(stk_moi, stk_x1_c, stk_x1_imp, stk_x2_c, stk_x2_imp)

formula <- list(y_moi, y_x1_classical, y_x1_imp, y_x2_classical, y_x2_imp) ~
  - 1 + beta.0 + beta.z +
  # First variable (x1)
  f(beta.x1, copy = "id.x1",
    hyper = list(beta = list(param = c(0, 1/1000), fixed = FALSE))) +
  f(id.x1, weight.x1, model = "iid", values = 1:n,
    hyper = list(prec = list(initial = -15, fixed = TRUE))) +
  alpha.x1.0 + alpha.x1.z +
  # Second variable (x2)
  f(beta.x2, copy = "id.x2",
    hyper = list(beta = list(param = c(0, 1/1000), fixed = FALSE))) +
  f(id.x2, weight.x2, model = "iid", values = 1:n,
    hyper = list(prec = list(initial = -15, fixed = TRUE))) +
  alpha.x2.0


prior.prec.y <- prior.prec.x1.u_c <-  prior.prec.x1 <- prior.prec.x2.u_c <- prior.prec.x2 <- c(10, 9)
initial.prec.y <- initial.prec.x1.u_c <- initial.prec.x1 <- initial.prec.x2.u_c <- initial.prec.x2 <- 1

model_two_errors <- inla(formula, data = inla.stack.data(stk_full),
                  family = c("gaussian", "gaussian", "gaussian", "gaussian", "gaussian"),
                  control.family = list(
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
                  ),
                  control.predictor = list(compute = TRUE)
)

summary(model_two_errors)


