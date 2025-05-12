# Part a
n <- 75 #number of patients
p <- 0.15 #prob of failure old method
y <- 0:n #possible number of failure
pmf <- dbinom(y, size=n, prob=p )
plot(y, pmf, type="h", lwd = 2, col = "steelblue",
     main = "Binomial PMF: False Negatives (p = 0.15)",
     xlab = "Number of False Negatives (y)",
     ylab = "Probability")

#part b
y <- 6
p_hat <- y/n
cat("Estimated failure probability (frequentist):", p_hat, "\n")

#part c
prior_mean <- 0.15
prior_sd <- 0.14

beta_params <- function(mean, sd) {
  var <- sd^2
  temp <- mean * (1 - mean) / var - 1
  alpha <- mean * temp
  beta  <- (1 - mean) * temp
  return(c(alpha = alpha, beta = beta))
}

prior_params <- beta_params(prior_mean, prior_sd)
alpha_prior <- prior_params["alpha"]
beta_prior <- prior_params["beta"]

alpha_post <- alpha_prior + y
beta_post  <- beta_prior + n - y

library(ggplot2)

p_vals <- seq(0, 0.4, length.out = 1000)
post_density <- dbeta(p_vals, alpha_post, beta_post)

post_mean <- alpha_post / (alpha_post + beta_post)
post_var  <- (alpha_post * beta_post) / ((alpha_post + beta_post)^2 * (alpha_post + beta_post + 1))

df <- data.frame(p=p_vals, density=post_density)

ggplot(df, aes(x = p, y = density)) +
  geom_line(color = "darkblue", size = 1) +
  geom_vline(xintercept = post_mean, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Posterior Distribution of Failure Probability (Bayesian)",
    subtitle = paste("Posterior Mean =", post_mean, 
                     " | Posterior Variance =", post_var),
    x = "Failure Probability p",
    y = "Density"
  ) +
  theme_minimal()

#part d
threshold <- 0.15
post_prob <- pbeta(threshold, alpha_post, beta_post)

cat("P(p < 0.15 | data) =", post_prob, "\n")

if (post_prob > 0.95) {
  cat("Reject H0: Evidence suggests the new test is better (failure rate < 15%)\n")
} else {
  cat("Do not reject H0: Not enough evidence that the new test is better\n")
}

df$region <- ifelse(df$p < threshold, "less_than_0.15", "greater_equal_0.15")

ggplot(df, aes(x = p, y = density)) +
  geom_area(aes(fill = region), alpha = 0.6) +
  geom_line(color = "black", size = 1) +
  geom_vline(xintercept = threshold, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = alpha_post / (alpha_post + beta_post), color = "blue", linetype = "dotted", linewidth = 1) +
  labs(
    title = "Posterior Distribution of Failure Rate p",
    subtitle = paste("Shaded area = P(p < 0.15) =", pbeta(threshold, alpha_post, beta_post)),
    x = "Failure Probability p",
    y = "Posterior Density",
    fill = "Region"
  ) +
  scale_fill_manual(values = c("less_than_0.15" = "steelblue", "greater_equal_0.15" = "gray80"),
                    labels = c("< 0.15", "≥ 0.15")) +
  theme_minimal()