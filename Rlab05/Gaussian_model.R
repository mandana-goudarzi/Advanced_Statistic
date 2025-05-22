library(rjags)
library(coda)

data_x <- c(13.427, 8.588, 10.908, 11.582, 11.011, 9.735, 13.779, 9.763)
n <- length(data_x)

model_string <- "
model {
  for (i in 1:N) {
    x[i] ~ dnorm(m, 1 / (s * s))
  }

  m ~ dunif(-10, 30)
  s ~ dnorm(0, 1/2500)T(0,)  # truncated to positive, sd = 50
  ratio <- m / s
}
"
writeLines(model_string, con = "gaussian_model.jags")

jags_data <- list(x = data_x, N = n)
params <- c("m", "s", "ratio")  

model <- jags.model(file = "gaussian_model.jags", data = jags_data, n.chains = 2, n.adapt = 500)
update(model, 1000)  

samples <- coda.samples(model, variable.names = params, n.iter = 5000)

summary(samples)

plot(samples)

samples_matrix <- as.matrix(samples)
hist(samples_matrix[, "ratio"], breaks = 50, col = "skyblue", main = "Posterior of m/s", xlab = "m/s")
# gelman-rubin diagnostic
gelman.diag(samples)

autocorr.diag(samples)
traceplot(samples[, "m"])
traceplot(samples[, "s"])
