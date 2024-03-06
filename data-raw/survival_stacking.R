# Exploring how to specify a measurement error model with survival moi

library(INLA)

n <- 100

t <- runif(n, 0, 10) # Survival time
d <- sample(c(0,1), n, replace = TRUE) # Survival event

x <- rnorm(n, 0, 1) # Covariate with measurement error
z <- rnorm(n, 0, 1) # Covariate without measurement error

data <- data.frame(t, d, x, z)

# Regression model of interest
# eta = beta.0 + beta.x*x + beta.z*z
stk_moi <- inla.stack(
    data = list(y_time = data$t, y_event = data$d),
    A = list(1),
    effects = list(
      list(beta.0 = rep(1, n),
           beta.x = 1:n,
           beta.z = data$z)),
    tag = "moi")

# Classical measurement error model
# x_obs = x + u_c
stk_c <- inla.stack(data = list(y_classical = data$x),
                    A = list(1),
                    effects = list(
                      list(id.x = 1:n,
                           weight.x = 1)),
                    tag = "classical")

# Imputation/exposure model
# 0 = x + alpha.0 + alpha.z + e_x
stk_imp <- inla.stack(data = list(y_imp = rep(0, n)),
                      A = list(1),
                      effects = list(
                        list(id.x = 1:n,
                             weight.x = rep(-1, n),
                             alpha.0 = rep(1, n),
                             alpha.z = data$z)),
                      tag = "imputation")

# Stack them on top of each other
stk_full <- inla.stack(stk_moi, stk_c, stk_imp)

# Formula
formula <- list(inla.surv(time = y_time, event = y_event), y_classical, y_imp) ~ - 1 + beta.0 + beta.z +
  f(beta.x, copy = "id.x",
    hyper = list(beta = list(param = c(0, 0.01), fixed = FALSE))) +
  f(id.x, weight.x, model = "iid", values = 1:n,
    hyper = list(prec = list(initial = -15, fixed = TRUE))) +
  alpha.0 + alpha.z

# Fit the model
mod <- inla(formula, data = inla.stack.data(stk_full),
            family = c("weibull.surv", "gaussian", "gaussian"),
            control.family = list(
              list(hyper = list(alpha = list(param = 0.01,
                                             initial = log(1.4),
                                             fixed = FALSE))),
              list(hyper = list(prec = list(initial = log(1),
                                            param = c(10, 9),
                                            fixed = FALSE))),
              list(hyper = list(prec = list(initial = log(1),
                                            param = c(10, 9),
                                            fixed = FALSE)))),
            control.predictor = list(compute = TRUE)
)


