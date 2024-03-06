set.seed(2024)
n <- 1000

# Covariate without error:
z <- rnorm(n, mean = 0, sd = 1)

# First covariate:
alpha.x1.0 <- 1; alpha.x1.z <- 2
x1 <- rnorm(n, mean = alpha.x1.0 + alpha.x1.z*z, sd = 1)

u_x1_c <- rnorm(n, sd = 1)
x1_obs <- x1 + u_x1_c

# Second covariate:
alpha.x2.0 <- 1
x2 <- rnorm(n, mean = alpha.x2.0, sd = 1)

u_x2_c <- rnorm(n, sd = 1)
x2_obs <- x2 + u_x2_c

# Response:
beta.0 <- 1; beta.x1 <- 2; beta.x2 <- 2; beta.z <- 2
y <- beta.0 + beta.x1*x1 + beta.x2*x2 + beta.z*z + rnorm(n)

two_error_data <- data.frame(y = y, x1 = x1_obs, x2 = x2_obs,
                             x1_true = x1, x2_true = x2, z = z)

usethis::use_data(two_error_data, overwrite = TRUE)
