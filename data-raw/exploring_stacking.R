
# Testing stacking

# SHADING EXAMPLE -----

## Stack Version -----

library(INLA)

data <- read.table(file = "data-raw/shading_data4supp.txt", header = TRUE)
attach(data)

n <- 60     # number of seedlings
s <- 15 	  # number of shadehouses
w <- w + rep(rnorm(s,0,1e-4),each=n/s)
w.red <- aggregate(w, by = list(sh), FUN = mean)[,2]
individual <- 1:n # id to incorporate individual random effects

prior.beta <- c(0,0.01)
prior.tau <- c(1,0.005)
prior.prec.u <- c(1.12,0.0203)

prec.tau <- 1/0.005
prec.u <- 1.12/0.0203

stk_1 = inla.stack(data = list(Y = cbind(y, NA)),
                   A = list(1),
                   effects = list(
                     list(beta.0 = rep(1,n),
                          beta.x = sh,
                          beta.z = z,
                          gamma = 1:n)),
                   tag = "est_1"
)

stk_2 = inla.stack(data = list(Y = cbind(NA, -w.red)),
                   A = list(1),
                   effects = list(
                     list(idx2.x = 1:s, weight2.x = rep(-1,s))),
                   tag = "est_2"
)

stk2 <- inla.stack(stk_1,stk_2)


formula2 <- Y ~  beta.0 - 1 +
  f(beta.x, copy = "idx2.x", values = 1:n,
    hyper = list(beta = list(param = prior.beta, fixed = FALSE))) +
  f(idx2.x, weight2.x, model = "iid",  values = 1:s,
    hyper = list(prec = list(initial = -15, fixed = TRUE))) +
  beta.z +
  f(gamma, model = "iid", values = 1:n,
    hyper = list(prec = list(initial = log(prec.tau), param = prior.tau)))


r.copy2 <- inla(formula2, data = inla.stack.data(stk2),
                family = c("poisson", "gaussian"),
                control.predictor=list(compute=TRUE, A = inla.stack.A(stk2)),
                control.family = list(
                  list(hyper = list()),
                  list(hyper = list(
                    prec = list(
                      initial=log(prec.u),
                      param = prior.prec.u,
                      fixed = FALSE)))),
                control.fixed = list(
                  mean.intercept = prior.beta[1],
                  prec.intercept = prior.beta[2],
                  mean = prior.beta[1],
                  prec = prior.beta[2])
)
#r.copy2 <- inla.hyperpar(r.copy2)
summary(r.copy2)
plot(r.copy2)

## Original code ----

Y <- matrix(NA, n+s, 2)
Y[1:n, 1] <- y
Y[n+(1:s), 2] <-  -w.red   ## -w.red was the original model..  meb type
beta.0 <- c(rep(1, n), rep(NA, s))
beta.x <- c(sh, rep(NA, s))
idx.x <- c(rep(NA, n), (1:s))
weight.x <- c(rep(NA, n), -rep(1, s))
beta.z <- c(z, rep(NA, s))
gamma <- c(1:n, rep(NA, s))

data.joint <- data.frame(Y, beta.0, beta.x, idx.x,  beta.z, gamma)

formula <- Y ~  beta.0 - 1 +
  f(beta.x, copy = "idx.x", values = 1:n,
    hyper = list(beta = list(param = prior.beta, fixed = FALSE))) +
  f(idx.x, weight.x, model = "iid", values = 1:s,
    hyper = list(prec = list(initial = -15, fixed = TRUE))) +
  beta.z +
  f(gamma, model = "iid", values = 1:n,
    hyper = list(prec = list(initial = log(prec.tau), param = prior.tau)))

r.copy <- inla(formula, data = data.joint,
               family = c("poisson", "gaussian"),
               control.family = list(
                 list(hyper = list()),
                 list(hyper = list(
                   prec = list(
                     initial=log(prec.u),
                     param = prior.prec.u,
                     fixed = FALSE)))),
               control.fixed = list(
                 mean.intercept = prior.beta[1],
                 prec.intercept = prior.beta[2],
                 mean = prior.beta[1],
                 prec = prior.beta[2])
)
#r.copy <- inla.hyperpar(r.copy)
summary(r.copy)
plot(r.copy)

detach(data)

# CLASSICAL ME SIMULATED DATA ----


data <- simple_data
attach(data)
n <- nrow(data)
## Original code ----

# Predictors for the sub-models
simple_moi <- y ~ x + z
simple_imp <- x ~ z

# Prior for beta.x
prior.beta <- c(0, 1/1000) # N(0, 10^3)

# Priors for y, measurement error and true x-value precision
prior.prec.y <- c(10, 9)
prior.prec.u_b <- c(10, 9)
prior.prec.u_c <- c(10, 9)
prior.prec.r <- c(10, 9)

# Initial values
initial.prec.y <- 1
initial.prec.u_b <- 1
initial.prec.u_c <- 1
initial.prec.r <- 1

# Fit the model
simple_model <- fit_inlamemi(data = data,
                           formula_moi = simple_moi,
                           formula_imp = simple_imp,
                           family_moi = "gaussian",
                           error_type = c("berkson", "classical"),
                           prior.prec.moi = prior.prec.y,
                           prior.prec.berkson = prior.prec.u_b,
                           prior.prec.classical = prior.prec.u_c,
                           prior.prec.imp = prior.prec.r,
                           initial.prec.moi = initial.prec.y,
                           initial.prec.berkson = initial.prec.u_b,
                           initial.prec.classical = initial.prec.u_c,
                           initial.prec.imp = initial.prec.r)
summary(simple_model)


## Stack version ----

# Regression model of interest
# y = beta.0 + beta.x*x_true + beta.z*z + e
stk_moi <- inla.stack(data = list(y_moi = y),
                      A = list(1),
                      effects = list(
                        list(beta.0 = rep(1, n),
                             beta.x = 1:n,
                             beta.z = z)),
                      tag = "moi")

# Berkson measurement error model
# 0 = -x_true + r + u_b
stk_b <- inla.stack(data = list(y_berkson = rep(0, n)),
                    A = list(1),
                    effects = list(
                      list(id.x = 1:n,
                           weight.x = -1,
                           id.r = 1:n,
                           weight.r = 1)),
                    tag = "berkson")

# Classical measurement error model
# x = r + u_c
stk_c <- inla.stack(data = list(y_classical = x),
                    A = list(1),
                    effects = list(
                      list(id.r = 1:n,
                           weight.r = 1)),
                    tag = "classical")

# Imputation/exposure model
# 0 = r + alpha.0 + alpha.z + e_r
stk_imp <- inla.stack(data = list(y_imp = rep(0, n)),
                      A = list(1),
                      effects = list(
                        list(id.r = 1:n,
                             weight.r = rep(-1, n),
                             alpha.0 = rep(1, n),
                             alpha.z = z)),
                      tag = "imputation")

# Stack them on top of each other
stk_full <- inla.stack(stk_moi, stk_b, stk_c, stk_imp)

formula <- list(y_moi, y_berkson, y_classical, y_imp) ~ - 1 + beta.0 + beta.z +
  f(beta.x, copy = "id.x",
    hyper = list(beta = list(param = c(0, 1/1000), fixed = FALSE))) +
  f(id.x, weight.x, model = "iid", values = 1:n,
    hyper = list(prec = list(initial = -15, fixed = TRUE))) +
  f(id.r, weight.r, model="iid", values = 1:n,
    hyper = list(prec = list(initial = -15, fixed = TRUE))) +
  alpha.0 + alpha.z

model_sim <- inla(formula, data = inla.stack.data(stk_full),
                  family = c("gaussian", "gaussian", "gaussian", "gaussian"),
                  control.family = list(
                    list(hyper = list(prec = list(initial = log(initial.prec.y),
                                                  param = prior.prec.y,
                                                  fixed = FALSE))),
                    list(hyper = list(prec = list(initial = log(initial.prec.u_b),
                                                  param = prior.prec.u_b,
                                                  fixed = FALSE))),
                    list(hyper = list(prec = list(initial = log(initial.prec.u_c),
                                                  param = prior.prec.u_c,
                                                  fixed = FALSE))),
                    list(hyper = list(prec = list(initial = log(initial.prec.r),
                                                  param = prior.prec.r,
                                                  fixed = FALSE)))
                  ),
                  control.predictor = list(compute = TRUE)
)

summary(model_sim)

