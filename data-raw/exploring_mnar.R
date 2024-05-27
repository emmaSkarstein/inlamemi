# Model for MNAR (sensitivity analysis)

library(inlamemi)
library(INLA)

n <- nrow(mar_data)

# Regression model of interest
# y = beta.0 + beta.x*x_true + beta.z*z + e
stk_moi <- inla.stack(data = list(y_moi = mar_data$y),
                      A = list(1),
                      effects = list(
                        list(beta.0 = rep(1, n),
                             beta.x = 1:n,
                             beta.z1 = mar_data$z1,
                             beta.z2 = mar_data$z2)),
                      tag = "moi")

# Classical measurement error model
# x = x_true + u_c
stk_c <- inla.stack(data = list(y_classical = mar_data$x),
                    A = list(1),
                    effects = list(
                      list(id.x = 1:n,
                           weight.x = 1)),
                    tag = "classical")

# Imputation/exposure model
# 0 = r + alpha.0 + alpha.z + e_r
stk_imp <- inla.stack(data = list(y_imp = rep(0, n)),
                      A = list(1),
                      effects = list(
                        list(id.x = 1:n,
                             weight.x = -1,
                             alpha.0 = rep(1, n),
                             alpha.z1 = mar_data$z1,
                             alpha.z2 = mar_data$z2)),
                      tag = "imputation")

stk_mis <- inla.stack(data = list(y_mis = as.numeric(is.na(mar_data$x))),
                      A = list(1),
                      effects = list(
                        list(gamma.x = 1:n,
                             gamma.0 = rep(1, n),
                             gamma.z1 = mar_data$z1,
                             gamma.z2 = mar_data$z2)),
                      tag = "missingness")

# Stack them on top of each other
stk_full <- inla.stack(stk_moi, stk_c, stk_imp, stk_mis)

formula <- list(y_moi, y_classical, y_imp, y_mis) ~ - 1 +
  beta.0 + beta.z1 + beta.z2 +
  f(beta.x, copy = "id.x",
    hyper = list(beta = list(param = c(0, 1/1000), fixed = FALSE))) +
  f(id.x, weight.x, model = "iid", values = 1:n,
    hyper = list(prec = list(initial = -15, fixed = TRUE))) +
  f(gamma.x, copy = "id.x",
    hyper = list(beta = list(param = c(0, 1/1000), fixed = FALSE))) +
  alpha.0 + alpha.z1 + alpha.z2 + gamma.0 + gamma.z1 + gamma.z2

model_mnar <- inla(formula, data = inla.stack.data(stk_full),
                  family = c("gaussian", "gaussian", "gaussian", "binomial"),
                  scale = c(rep(1, n), rep(10^8, n), rep(1, n), rep(1, n)),
                  control.family = list(
                    list(hyper = list(prec = list(initial = log(1),
                                                  param = c(10, 9),
                                                  fixed = FALSE))),
                    list(hyper = list(prec = list(initial = log(1),
                                                  param = c(10, 9),
                                                  fixed = FALSE))),
                    list(hyper = list(prec = list(initial = log(1),
                                                  param = c(10, 9),
                                                  fixed = FALSE))),
                    list()),
                  control.predictor = list(compute = TRUE)
)

summary(model_mnar)
