## Air pollution example

#  |Error types | Likelihood | Response | Covariate with error | Other covariate(s) |
#  |:-----------|:----------|:--------|:----------|:------------|
#  |Berkson, classical | Binomial | asthma | airpollution | gender, age, district |
#
#  In this example, we simulate data from a fictitious, but realistic scenario where we want examine if people in areas with high air pollution seem to have asthma more than in areas with lower air pollution. But the air pollution value for the people in the study cannot be observed directly, instead sensors have been placed at certain locations, and the value for a person is set to be the value gathered at the closest sensor. This leads to a Berkson error. In addition to that, we believe that there is some noise in the measurements due to imprecision in the sensor. This corresponds to a classical measurement error.

### Generating the data

set.seed(2034)
n <- 1000
gender <- sample(c(0,1), n, replace = TRUE)
age <- rgamma(n, shape = 10, scale = 3)
n_districts <- 10
district <- rep(seq(1:n_districts), each = n/n_districts)
district_re <- rep(rnorm(n_districts), each = n/n_districts)

alpha.0 <- 3
pollution_b <- alpha.0 + district_re +
  rnorm(n, mean = 0, sd = 1) # Berkson error
pollution <- pollution_b + rnorm(n, 0, 1) # Classical and Berkson error (this is what is observed)
pollution_correct <- pollution_b + rnorm(n, mean = 0, sd = 1) # Correct

beta.0 <- -8; beta.pollution <- 2; beta.gender <- -2; beta.age <- 0.5

asthma_predictor <- beta.0 + beta.pollution*pollution_correct +
  beta.gender*gender + beta.age*age
asthma_prob <- exp(asthma_predictor)/(1 + exp(asthma_predictor))
asthma <- rbinom(n, 1, prob = asthma_prob)

airpollution <- data.frame(asthma = asthma,
                           pollution = pollution,
                           district,
                           age,
                           gender)


### Fitting the model

# Scale the data
airpollution_scaled <- airpollution %>%
  mutate(across(c(pollution, age), ~ c(scale(., scale = FALSE))))

airpollution_model <- fit_inlamemi(data = airpollution_scaled,
                                   formula_moi = asthma ~ pollution + age + gender,
                                   formula_imp = pollution ~ f(district, model="iid"),
                                   family_moi = "binomial",
                                   error_type = c("berkson", "classical"),
                                   prior.prec.berkson = c(10, 9),
                                   prior.prec.classical = c(10, 9),
                                   prior.prec.imp = c(10, 9),
                                   prior.beta.error = c(0, 1/1000),
                                   initial.prec.berkson = 1,
                                   initial.prec.classical = 1,
                                   initial.prec.imp = 1)

airpollution_summary <- summary(airpollution_model)

airpollution.truth <- tibble::tribble(
  ~"variable", ~"value",
  "beta.0",         beta.0,
  "beta.pollution", beta.pollution,
  "beta.gender",    beta.gender,
  "beta.age",       beta.age
)

plot(airpollution_model, plot_intercepts = FALSE) +
  geom_point(data = airpollution.truth, aes(x = value))

naive_mod <- glm(asthma ~ pollution + age + gender, data = airpollution_scaled)
summary(naive_mod)
