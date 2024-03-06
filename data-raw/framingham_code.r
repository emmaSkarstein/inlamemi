###################################################################
## Measurement error in GLMMs with INLA (2013)
## by S. Muff, A. Riebler, H. Rue, P. Saner and  L. Held
##
## r-inla code for Section 5.2
## Influence of systolic blood pressure on coronary heart disease
###################################################################

library(INLA)
# > inla.version()
#
#
# INLA build date .........: Sat Jul 13 09:42:38 CEST 2013
# INLA hgid ...............: hgid: 93d466d9eaab  date: Sat Jul 13 09:42:00 2013 +0200
# INLA-program hgid .......: hgid: ef654eda81ec  date: Thu Jul 11 01:03:19 2013 +0200
# Maintainers .............: Havard Rue <hrue@math.ntnu.no>
#   : Finn Lindgren <finn.lindgren@gmail.com>
#   : Daniel Simpson <dp.simpson@gmail.com>
#   : Andrea Riebler <andrea.riebler@math.ntnu.no>
#   Web-page ................: http://www.r-inla.org
# Email support ...........: help@r-inla.org
# : r-inla-discussion-group@googlegroups.com
# Source-code .............: http://inla.googlecode.com

data <- read.table("data-raw/framingham.txt", header=T)
attach(data)
n <- nrow(data)

prior.beta <- c(0, 0.01)
prior.alpha0 <- c(0, 1)
prior.alphaz <- c(0, 1)
prior.prec.x <- c(10, 1)
prior.prec.u <- c(100, 1)

# initial values (mean of prior)
prec.u <- 100
prec.x <- 10


Y <- matrix(NA, 4*n, 3)
Y[1:n, 1] <- y
Y[n+(1:n), 2] <- rep(0, n)
Y[2*n+(1:n), 3] <- w1
Y[3*n+(1:n), 3] <- w2

beta.0 <- c(rep(1, n), rep(NA, n), rep(NA, n), rep(NA, n))
beta.x <- c(1:n, rep(NA, n), rep(NA, n), rep(NA, n))
idx.x <- c(rep(NA, n), 1:n, 1:n, 1:n)
weight.x <- c(rep(1, n), rep(-1, n), rep(1, n), rep(1,n))
beta.z <- c(z, rep(NA, n), rep(NA, n), rep(NA,n))
alpha.0 <- c(rep(NA, n), rep(1, n), rep(NA, n), rep(NA, n))
alpha.z <- c(rep(NA, n), z, rep(NA, n), rep(NA, n))

Ntrials <- c(rep(1, n), rep(NA, n), rep(NA, n), rep(NA, n))

data.joint <- data.frame(Y=Y,
                         beta.0=beta.0, beta.x=beta.x, beta.z=beta.z,
                         idx.x=idx.x, weight.x=weight.x,
                         alpha0=alpha.0, alpha.z=alpha.z,
                         Ntrials=Ntrials)

formula <- Y ~  f(beta.x, copy = "idx.x",
                  hyper = list(beta = list(param = prior.beta, fixed = FALSE))) +
                f(idx.x, weight.x, model = "iid", values = 1:n,
                  hyper = list(prec = list(initial = -15, fixed = TRUE))) +
                beta.0 - 1 + beta.z + alpha.0 + alpha.z

r <- INLA::inla(formula, Ntrials = Ntrials, data = data.joint,
          family = c("binomial", "gaussian", "gaussian"),
          control.family = list(
            list(hyper = list()),
            list(hyper = list(
              prec = list(initial = log(prec.x),
                          param = prior.prec.x,
                          fixed = FALSE))),
            list(hyper = list(
              prec = list(initial=log(prec.u),
                          param = prior.prec.u,
                          fixed = FALSE)))),
          control.fixed = list(
              mean = list(beta.0=prior.beta[1], beta.z=prior.beta[1],
                        alpha.z=prior.alphaz[1], alpha.0=prior.alpha0[1]),
              prec = list(beta.0=prior.beta[2], beta.z=prior.beta[2],
                        alpha.z=prior.alphaz[2], alpha.0=prior.alpha0[2]))
)
r <-inla.hyperpar(r)

summary(r)
plot(r)



