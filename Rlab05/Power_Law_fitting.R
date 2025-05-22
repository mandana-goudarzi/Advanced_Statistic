set.seed(42)

n <- 100           
xmin <- 10         
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

plot(density(posterior_uniform[, "alpha"]), main = "Posterior Densities")
lines(density(posterior_normal[, "alpha"]), col = "green")
abline(v = alpha_mle, col = "red", lty = 2)
library(modeest)

posterior_uniform <- as.matrix(samples_uniform)
posterior_normal  <- as.matrix(samples_normal)


#for Uniform Prior
cat("=== Bayesian Results (Uniform Prior) ===\n")
mean_uniform <- mean(posterior_uniform[, "alpha"])
mode_uniform <- as.numeric(mlv(posterior_uniform[, "alpha"], method = "parzen"))
ci_uniform <- quantile(posterior_uniform[, "alpha"], probs = c(0.025, 0.975))

cat("Posterior Mean: ", mean_uniform, "\n")
cat("Posterior Mode: ", mode_uniform, "\n")
cat("95% Credible Interval: ", ci_uniform, "\n\n")

#for Normal Prior
cat("=== Bayesian Results (Normal Prior) ===\n")
mean_normal <- mean(posterior_normal[, "alpha"])
mode_normal <- as.numeric(mlv(posterior_normal[, "alpha"], method = "parzen"))
ci_normal <- quantile(posterior_normal[, "alpha"], probs = c(0.025, 0.975))

cat("Posterior Mean: ", mean_normal, "\n")
cat("Posterior Mode: ", mode_normal, "\n")
cat("95% Credible Interval: ", ci_normal, "\n")

alpha_mle <- 1 + length(x) / sum(log(x / xmin))

cat("=== Frequentist MLE Estimate ===\n")
cat("MLE alpha: ", alpha_mle, "\n\n")

mean_uniform <- mean(posterior_uniform[, "alpha"])
mode_uniform <- as.numeric(mlv(posterior_uniform[, "alpha"], method = "parzen"))
ci_uniform <- quantile(posterior_uniform[, "alpha"], probs = c(0.025, 0.975))

mean_normal <- mean(posterior_normal[, "alpha"])
mode_normal <- as.numeric(mlv(posterior_normal[, "alpha"], method = "parzen"))
ci_normal <- quantile(posterior_normal[, "alpha"], probs = c(0.025, 0.975))


cat("=== Bayesian Estimates (Uniform Prior) ===\n")
cat("Mean: ", mean_uniform, "\n")
cat("Mode: ", mode_uniform, "\n")
cat("95% Credible Interval: ", ci_uniform, "\n\n")

cat("=== Bayesian Estimates (Normal Prior) ===\n")
cat("Mean: ", mean_normal, "\n")
cat("Mode: ", mode_normal, "\n")
cat("95% Credible Interval: ", ci_normal, "\n\n")


samples_df <- data.frame(
  alpha = c(posterior_uniform[, "alpha"], posterior_normal[, "alpha"]),
  prior = rep(c("Uniform", "Normal"), each = nrow(posterior_uniform))
)

library(ggplot2)

ggplot(samples_df, aes(x = alpha, fill = prior)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  geom_vline(xintercept = alpha_mle, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Bayesian vs Frequentist Estimates of α",
       x = expression(alpha),
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen")) +
  theme(legend.title = element_blank())