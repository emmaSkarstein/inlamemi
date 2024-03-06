# Exploring missingnes only
# Emma Skarstein, January 2024

library(INLA)
library(inlamemi)
library(ggplot2)
library(dplyr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data generation ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(2024)
n <- 1000

# Covariate without missingness:
z <- rnorm(n, mean = 0, sd = 1)

# Covariate that will have missingness:
alpha.0 <- 1; alpha.z <- 2
x <- rnorm(n, mean = alpha.0 + alpha.z*z, sd = 1)

# Response:
beta.0 <- 1; beta.x <- 2; beta.z <- 2
y <- beta.0 + beta.x*x + beta.z*z + rnorm(n)

# Missingness:
m_pred <- -1.5 - 0.5*z # This gives a mean probability of missing of ca 0.2.
m_prob <- exp(m_pred)/(1 + exp(m_pred))

m_index <- as.logical(rbinom(n, 1, prob = m_prob)) # MAR
# m_index <- sample(1:n, 0.2*n, replace = FALSE) # MCAR
x_obs <- x
x_obs[m_index] <- NA

missing_data <- data.frame(y = y, x = x_obs, z = z)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Naive model ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

naive_model <- inla(formula = y ~ x + z, family = "gaussian", data = missing_data[!is.na(missing_data$x), ])

naive_model$summary.fixed
naive_model$summary.hyperpar


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Measurement error/missing data model ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Regression model of interest
# y = beta.0 + beta.x*x_true + beta.z*z + e
stk_moi <- inla.stack(data = list(y_moi = y),
                      A = list(1),
                      effects = list(
                        list(beta.0 = rep(1, n),
                             beta.x = 1:n,
                             beta.z = z)),
                      tag = "moi")

# Classical measurement error model
# x = r + u_c
stk_c <- inla.stack(data = list(y_classical = x),
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
                             weight.x = rep(-1, n),
                             alpha.0 = rep(1, n),
                             alpha.z = z)),
                      tag = "imputation")

# Stack them on top of each other
stk_full <- inla.stack(stk_moi, stk_c, stk_imp)

formula <- list(y_moi, y_classical, y_imp) ~ - 1 + beta.0 + beta.z +
  f(beta.x, copy = "id.x",
    hyper = list(beta = list(param = c(0, 1/1000), fixed = FALSE))) +
  f(id.x, weight.x, model = "iid", values = 1:n,
    hyper = list(prec = list(initial = -15, fixed = TRUE))) +
  alpha.0 + alpha.z

cont_fam1 <- list(
  list(hyper = list(prec = list(initial = log(1),
                                param = c(10, 9),
                                fixed = FALSE))),
  list(hyper = list(prec = list(initial = log(1),
                                param = c(10, 9),
                                fixed = FALSE))),
  list(hyper = list(prec = list(initial = log(1),
                                param = c(10, 9),
                                fixed = FALSE)))
)

model_ME <- inla(formula, data = inla.stack.data(stk_full),
                  family = c("gaussian", "gaussian", "gaussian"),
                  scale = c(rep(1, n), rep(1, n), rep(1, n)),
                  control.family = cont_fam1,
                  control.predictor = list(compute = TRUE))
summary(model_ME)

model_MI <- inla(formula, data = inla.stack.data(stk_full),
                 family = c("gaussian", "gaussian", "gaussian"),
                 scale = c(rep(1, n), rep(10^4, n), rep(1, n)),
                 control.family = cont_fam1,
                 control.predictor = list(compute = TRUE))
summary(model_MI)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comparison ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract the relevant stuff
class(model_ME) <- "inlamemi"
class(model_MI) <- "inlamemi"


plot(model_ME, plot_intercept = FALSE) +
  scale_x_continuous(limits = c(1, 2.5))
plot(model_MI, plot_intercept = FALSE) +
  scale_x_continuous(limits = c(1, 2.5))

summary(model_ME)
summary(model_MI)

ME_components <- simplify_inlamemi_model_summary(model_ME)
MI_components <- simplify_inlamemi_model_summary(model_MI)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Doing it many times ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit_MI <- function(error_scalar, cont_fam, scale_present_only = FALSE){
  if(scale_present_only){
    missing_loc <- is.na(stk_full$data$data$y_classical)[1000:1999]
    ME_scale <- rep(1, n)
    ME_scale[!missing_loc] <- error_scalar
  }else{
    ME_scale <- rep(error_scalar, n)
  }
  scale_vec <- c(rep(1, n), ME_scale, rep(1, n))
  data_for_inla <- inla.stack.data(stk_full, scale = scale_vec)
  model_MI <- inla(formula, data = data_for_inla,
                     family = c("gaussian", "gaussian", "gaussian"),
                     scale = scale,
                     control.family = cont_fam,
                     control.predictor = list(compute = TRUE))


  beta_MI <- model_MI$summary.hyperpar["Beta for beta.x",]
  beta_MI$error_scale <- as.character(error_scalar)
  return(beta_MI)
}

test_mod <- fit_MI(error_scalar = 10, cont_fam = cont_fam2)

niter <- 10
results <- list()

for(i in 1:niter){
  betas <- sapply(list(1, 10^2, 10^6, 10^8, 10^10, 10^12), fit_MI,
                  stk_full = stk_full, cont_fam = cont_fam1)
  betas_trans <- as.data.frame(t(betas))

  naive_model <- inla(formula = y ~ x + z, family = "gaussian", data = missing_data)
  beta_naive <- naive_model$summary.fixed[2, 1:6]
  beta_naive$error_scale <- "naive"

  all_betas <- rbind(betas_trans, beta_naive)
  all_betas$iteration <- as.factor(i)
  results <- rbind(results, all_betas)
}

results_df <- as.data.frame(lapply(results, unlist))

#str(results_df)

ggplot(results_df, aes(x = mean, y = iteration, color = error_scale)) +
  geom_vline(xintercept = 2) +
  geom_point() +
  geom_linerange(aes(xmin = `X0.025quant`, xmax = `X0.975quant`)) +
  facet_wrap(~ error_scale) +
  theme_bw()

ggsave("data-raw/missing_only.png")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fixed ME variance and only scaling observed values
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cont_fam1 <- list(
  list(hyper = list(prec = list(initial = log(1),
                                param = c(10, 9),
                                fixed = FALSE))),
  list(hyper = list(prec = list(initial = log(1),
                                param = c(10, 9),
                                fixed = FALSE))),
  list(hyper = list(prec = list(initial = log(1),
                                param = c(10, 9),
                                fixed = FALSE)))
)

cont_fam2 <- list(
  list(hyper = list(prec = list(initial = log(1),
                                param = c(10, 9),
                                fixed = FALSE))),
  list(hyper = list(prec = list(initial = log(1),
                                param = c(10, 9),
                                fixed = TRUE))),
  list(hyper = list(prec = list(initial = log(1),
                                param = c(10, 9),
                                fixed = FALSE)))
)


niter <- 3
results <- list()

for(i in 1:niter){
  betas_default <- sapply(list(10^2, 10^6, 10^8), fit_MI, cont_fam = cont_fam1)
  betas_fixed_ME <- sapply(list(10^2, 10^6, 10^8), fit_MI, cont_fam = cont_fam2)
  betas_scale_present <- sapply(list(10^2, 10^6, 10^8), fit_MI,
                                     cont_fam = cont_fam1, scale_present_only = TRUE)

  default <- as.data.frame(t(betas_default))
  fixed_ME <- as.data.frame(t(betas_fixed_ME))
  scale_present <- as.data.frame(t(betas_scale_present))


  all_betas <- bind_rows(default, fixed_ME, scale_present, .id = "type")
  all_betas$iteration <- as.factor(i)
  results <- rbind(results, all_betas)
}

results_df2 <- as.data.frame(lapply(results, unlist))

ggplot(results_df2, aes(x = mean, y = iteration, color = type)) +
  geom_vline(xintercept = 2) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_linerange(aes(xmin = `X0.025quant`, xmax = `X0.975quant`), alpha = 0.7,
                 position = position_dodge(width = 0.3)) +
  facet_wrap(~ error_scale) +
  theme_bw()
