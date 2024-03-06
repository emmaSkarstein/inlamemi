## code to prepare `simple_data` dataset goes here

set.seed(2024)
n <- 1000

# Covariate without error:
z <- rnorm(n, mean = 0, sd = 1)

# Berkson error:
u_b <- rnorm(n, sd = 1)
alpha.0 <- 1; alpha.z <- 2
r <- rnorm(n, mean = alpha.0 + alpha.z*z, sd = 1)
x <- r + u_b # Turn off Berkson by commenting out "+ u_b"

# Response:
beta.0 <- 1; beta.x <- 2; beta.z <- 2
y <- beta.0 + beta.x*x + beta.z*z + rnorm(n)

# Classical error:
u_c <- rnorm(n, sd = 1)
x_obs <- r + u_c

# Missingness:
m_pred <- -1.5 - 0.5*z # This gives a mean probability of missing of ca 0.2.
m_prob <- exp(m_pred)/(1 + exp(m_pred))

m_index <- as.logical(rbinom(n, 1, prob = m_prob)) # MAR
# m_index <- sample(1:n, 0.2*n, replace = FALSE) # MCAR
x_obs[m_index] <- NA

simple_data <- data.frame(y = y, x = x_obs, x_true = x, z = z)

usethis::use_data(simple_data, overwrite = TRUE)
