library(rstan)
library(ggplot2)

X <- c(105.5203, 227.4457, 405.6937, 661.2858, 886.1422,
       1189.6514, 1631.8262, 1951.8381, 2528.5246)
Y <- c(15.40459, 36.05480, 51.84040, 94.66744, 123.79828,
       216.99935, 173.83507, 318.46511, 427.27788)
sigma <- c(15.40459, 36.05480, 51.84040, 94.66744, 123.79828,
           216.99935, 173.83507, 318.46511, 427.27788)

N <- length(X)
stan_code <- "
data {
  int<lower=1> N;
  vector[N] X;
  vector[N] Y;
  vector[N] sigma;
}
parameters {
  real beta0;
  real<lower=-1, upper=1> beta1;
  real beta2;
}
model {
  // Priors
  beta0 ~ normal(5, 1);
  beta2 ~ normal(30, 15);
  
  // Likelihood
  for (n in 1:N) {
    Y[n] ~ normal(beta0 + beta1 * X[n] + beta2 * square(X[n]), sigma[n]);
  }
}
"

writeLines(stan_code, "quadratic_regression.stan")
stan_data <- list(N = N, X = X, Y = Y, sigma = sigma)

fit <- stan(file = "quadratic_regression.stan",
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 42)

print(fit, pars = c("beta0", "beta1", "beta2"), probs = c(0.025, 0.975))
posterior <- extract(fit)
beta0_hat <- mean(posterior$beta0)
beta1_hat <- mean(posterior$beta1)
beta2_hat <- mean(posterior$beta2)


X_seq <- seq(min(X), max(X), length.out = 100)
Y_fit <- beta0_hat + beta1_hat * X_seq + beta2_hat * X_seq^2


df_data <- data.frame(X = X, Y = Y, sigma = sigma)
df_fit <- data.frame(X = X_seq, Y = Y_fit)

ggplot() +
  geom_point(data = df_data, aes(x = X, y = Y)) +
  geom_errorbar(data = df_data, aes(x = X, ymin = Y - sigma, ymax = Y + sigma), width = 0) +
  geom_line(data = df_fit, aes(x = X, y = Y), color = "blue") +
  labs(title = "Quadratic Bayesian Regression Fit", x = "X", y = "Y")
ci_beta0 <- quantile(posterior$beta0, probs = c(0.025, 0.975))
ci_beta1 <- quantile(posterior$beta1, probs = c(0.025, 0.975))
ci_beta2 <- quantile(posterior$beta2, probs = c(0.025, 0.975))

cat("95% Credibility Intervals:\n")
cat("beta0:", ci_beta0, "\n")
cat("beta1:", ci_beta1, "\n")
cat("beta2:", ci_beta2, "\n")
