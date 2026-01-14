#Getting started in JAGS using R
#Install and load packages
#install.packages(c("rjags", "coda"))
library(rjags)
library(coda)
#Minimal working example (Bayesian linear regression)
#a)example using fake data
set.seed(1)
n <- 80
x <- rnorm(n)
y <- 1.5 + 2.0*x + rnorm(n, sd = 1)

data_jags <- list(
  n = n,
  x = x,
  y = y
)
#b)(b) Write the JAGS model
model_string <- "
model{
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- a + b*x[i]
  }

  # priors
  a ~ dnorm(0, 0.001)
  b ~ dnorm(0, 0.001)

  tau <- 1 / (sigma*sigma)
  sigma ~ dunif(0, 10)
}
"
writeLines(model_string, con = "linreg.jags")
#c) Fit the model using JAGS
library(rjags)
library(coda)

params <- c("a", "b", "sigma")

m <- jags.model(
  file = "linreg.jags",
  data = data_jags,
  n.chains = 3,
  n.adapt = 1000
)

update(m, 2000)  # burn-in

samp <- coda.samples(
  model = m,
  variable.names = params,
  n.iter = 5000,
  thin = 2
)
#d) Check convergence and summarize results
summary(samp)
plot(samp)
gelman.diag(samp)
effectiveSize(samp)

