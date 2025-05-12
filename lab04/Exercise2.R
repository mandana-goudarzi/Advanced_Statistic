#part a

x <- c(0.269, 0.344, 0.802, 0.418, 0.433, 0.835, 0.52, 0.139, 0.243, 0.294,
       0.723, 0.493, 0.504, 0.428, 0.27, 0.53, 0.057, 0.585, 0.288, 0.171)

alpha <- 2
beta_grid <- seq(1, 10, length.out = 1000)

log_likelihood <- sapply(beta_grid, function(b){
  sum(dbeta(x, shape1=alpha, shape2=b, log=TRUE))
  })

prior <- sapply(beta_grid, function(b){
  if (b >= 0.1 && b < 2) {
    return(0.2 * b)
  } else if (b >= 2 && b < 4) {
    return((1 / (0.8 * sqrt(2 * pi))) * exp(-((b - 2.8)^2) / (2 * 0.8^2)))
  } else if (b >= 4 && b <= 10) {
    return(0.1)
  } else {
    return(0)
  }
})

# unnormalized posterior: prior × likelihood
unnorm_posterior <- prior * exp(log_likelihood)

# Normalize the posterior
posterior <- unnorm_posterior / sum(unnorm_posterior * diff(beta_grid)[1])

posterior_mean <- sum(beta_grid * posterior * diff(beta_grid)[1])
posterior_sd <- sqrt(sum((beta_grid - posterior_mean)^2 * posterior * diff(beta_grid)[1]))

cat("Posterior mean of beta:", posterior_mean, "\n")
cat("Posterior SD of beta:", posterior_sd, "\n")

#part b
delta_beta <- diff(beta_grid)[1]
posterior_cdf <- cumsum(posterior * delta_beta)

lower_index <- which.min(abs(posterior_cdf - 0.025))
upper_index <- which.min(abs(posterior_cdf - 0.975))

beta_lower <- beta_grid[lower_index]
beta_upper <- beta_grid[upper_index]

cat("95% Credibility Interval for beta:\n")
cat("[", beta_lower, ",", beta_upper, "]\n")

#part c
library(ggplot2)

df <- data.frame(
  beta = beta_grid,
  density = posterior
)


ggplot(df, aes(x = beta, y = density)) +
  geom_line(color = "steelblue", size = 1.2) +
  
  # Mean line
  geom_vline(xintercept = posterior_mean, color = "red", linetype = "dashed", linewidth = 1.2) +
  
  # SD area (mean ± 1 sd)
  geom_vline(xintercept = posterior_mean - posterior_sd, color = "darkgreen", linetype = "dotted", linewidth = 1) +
  geom_vline(xintercept = posterior_mean + posterior_sd, color = "darkgreen", linetype = "dotted", linewidth = 1) +
  
  # 95% credibility interval
  geom_vline(xintercept = beta_lower, color = "purple", linetype = "dotdash", linewidth = 1) +
  geom_vline(xintercept = beta_upper, color = "purple", linetype = "dotdash", linewidth = 1) +
  
  annotate("text", x = posterior_mean, y = max(posterior) * 0.9, label = "Mean", color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = posterior_mean - posterior_sd, y = max(posterior) * 0.8, label = "-1 SD", color = "darkgreen", angle = 90, vjust = -0.5) +
  annotate("text", x = posterior_mean + posterior_sd, y = max(posterior) * 0.8, label = "+1 SD", color = "darkgreen", angle = 90, vjust = -0.5) +
  annotate("text", x = beta_lower, y = max(posterior) * 0.7, label = "2.5%", color = "purple", angle = 90, vjust = -0.5) +
  annotate("text", x = beta_upper, y = max(posterior) * 0.7, label = "97.5%", color = "purple", angle = 90, vjust = -0.5) +
  
  labs(
    title = "Posterior Distribution of β",
    x = "β",
    y = "Posterior Density"
  ) +
  theme_minimal()
