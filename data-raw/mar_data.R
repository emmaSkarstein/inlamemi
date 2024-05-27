## code to prepare `mar_data` dataset goes here

set.seed(2024)
n <- 1000

z1 <- rnorm(n, mean = 0, sd = 1)
z2 <- rnorm(n, mean = 0, sd = 1)

x <- rnorm(n, mean = 1 + 0.3*z1, sd = 1)

m_pred <- -1.5 - 0.5*z2
m_prob <- exp(m_pred)/(1 + exp(m_pred))
m <- as.logical(rbinom(n, 1, prob = m_prob))
x_obs <- x
x_obs[m] <- NA

sum(is.na(x_obs))/length(x_obs)

beta.0 <- 1; beta.x <- 2; beta.z1 <- 2; beta.z2 <- 2
y <- beta.0 + beta.x*x + beta.z1*z1 + beta.z2*z2 + rnorm(n)

mar_data <- data.frame(y = y, x = x_obs, x_true = x, z1 = z1, z2 = z2)

usethis::use_data(mar_data, overwrite = TRUE)
