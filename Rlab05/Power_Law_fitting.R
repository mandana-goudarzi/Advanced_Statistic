set.seed(42)

n <- 100           
xmin <- 10         # min value
alpha <- 2.5       

# Inverse transform sampling
# F(x) = 1 - (x_min / x)^(alpha - 1)
# x: x = xmin * (1 - U)^(-1 / (alpha - 1))

u <- runif(n)  # uniform random numbers between 0 and 1
x <- xmin * (1 - u)^(-1 / (alpha - 1))

head(x)
summary(x)

hist(x, breaks = 30, col = "skyblue", main = "Synthetic Power-Law Data", xlab = "x")

model_uniform <- "
model {
  for (i in 1:N) {
    zeros[i] ~ dpois(phi[i])
    phi[i] <- -log(alpha - 1) + log(xmin) + alpha * log(x[i] / xmin)
  }

  alpha ~ dunif(1.01, 10)
}
"
writeLines(model_uniform, con = "powerlaw_uniform.jags")

model_normal <- "
model {
  for (i in 1:N) {
    zeros[i] ~ dpois(phi[i])
    phi[i] <- -log(alpha - 1) + log(xmin) + alpha * log(x[i] / xmin)
  }

  alpha ~ dnorm(2.5, 1/100)
}
"
writeLines(model_normal, con = "powerlaw_normal.jags")

zeros <- rep(0, length(x))  # required for the zeros-trick
data_list <- list(x = x, N = length(x), xmin = xmin, zeros = zeros)

samples_uniform <- run_jags_model("powerlaw_uniform.jags", data_list)
samples_normal  <- run_jags_model("powerlaw_normal.jags", data_list)

library(rjags)
library(coda)

summary(samples_uniform)
summary(samples_normal)

alpha_hat <- 1 + length(x) / sum(log(x / xmin))
alpha_hat

hist(as.matrix(samples_uniform), col = "lightblue", breaks = 40, main = "Posterior (Uniform Prior)", xlab = "alpha")
abline(v = alpha, col = "red", lwd = 2, lty = 2)        # True alpha
abline(v = alpha_hat, col = "blue", lwd = 2, lty = 3)   # Frequentist MLE

hist(as.matrix(samples_normal), col = "lightgreen", breaks = 40, main = "Posterior (Normal Prior)", xlab = "alpha")
abline(v = alpha, col = "red", lwd = 2, lty = 2)
abline(v = alpha_hat, col = "blue", lwd = 2, lty = 3)

