library(binom)

#a) Frequentist estimator for p
n1 <- 115
y1 <- 10
p_hat1 <- y1 / n1
cat("Frequentist estimate of p (sample 1):", p_hat1, "\n")

# b): Posterior with Beta(1,11) prior

a_prior <- 1
b_prior <- 11

a_post1 <- a_prior + y1
b_post1 <- b_prior + n1 - y1

cat("Posterior distribution (Beta): Beta(", a_post1, ",", b_post1, ")\n")

# c): Posterior mean, variance, 95% credible interval

post_mean1 <- a_post1 / (a_post1 + b_post1)
post_var1 <- (a_post1 * b_post1) / ((a_post1 + b_post1)^2 * (a_post1 + b_post1 + 1))
ci1 <- qbeta(c(0.025, 0.975), a_post1, b_post1)

cat("Bayesian estimator (mean):", post_mean1, "\n")
cat("Bayesian variance:", post_var1, "\n")
cat("95% credible interval:", ci1, "\n")

# d): Hypothesis test (H0: p = 0.1 vs H1: p ≠ 0.1)

# Frequentist: Binomial test
freq_test1 <- binom.test(y1, n1, p = 0.1, alternative = "two.sided")
print(freq_test1)

# Bayesian: check if 0.1 is within credible interval
if (0.1 < ci1[1] || 0.1 > ci1[2]) {
  cat("Bayesian: Reject H0 at 5% level\n")
} else {
  cat("Bayesian: Do not reject H0 at 5% level\n")
}


n2 <- 165
y2 <- 9


# e) Frequentist estimator for p (second sample)

p_hat2 <- y2 / n2
cat("\nFrequentist estimate of p (sample 2):", p_hat2, "\n")

# f) Update prior using posterior from part (b)

a_prior2 <- a_post1
b_prior2 <- b_post1

a_post2 <- a_prior2 + y2
b_post2 <- b_prior2 + n2 - y2

cat("Updated posterior after second sample: Beta(", a_post2, ",", b_post2, ")\n")

# g) Posterior mean, variance, 95% credible interval

post_mean2 <- a_post2 / (a_post2 + b_post2)
post_var2 <- (a_post2 * b_post2) / ((a_post2 + b_post2)^2 * (a_post2 + b_post2 + 1))
ci2 <- qbeta(c(0.025, 0.975), a_post2, b_post2)

cat("Bayesian estimator (mean):", post_mean2, "\n")
cat("Bayesian variance:", post_var2, "\n")
cat("95% credible interval:", ci2, "\n")

# h) Hypothesis test for second sample

# Frequentist
freq_test2 <- binom.test(y2, n2, p = 0.1, alternative = "two.sided")
print(freq_test2)

# Bayesian
if (0.1 < ci2[1] || 0.1 > ci2[2]) {
  cat("Bayesian: Reject H0 at 5% level (second sample)\n")
} else {
  cat("Bayesian: Do not reject H0 at 5% level (second sample)\n")
}

library(ggplot2)

plot_posterior <- function(a, b, title_text) {
  x_vals <- seq(0, 0.25, length.out = 1000)
  y_vals <- dbeta(x_vals, a, b)
  df <- data.frame(p = x_vals, density = y_vals)
  
  ggplot(df, aes(x = p, y = density)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = 0.1, color = "red", linetype = "dashed") +
    labs(title = title_text,
         subtitle = "Red dashed line = H0: p = 0.1",
         x = "p (probability)", y = "Density") +
    theme_minimal()
}

plot1 <- plot_posterior(a_post1, b_post1, "Posterior Distribution (First Sample)")
print(plot1)

plot2 <- plot_posterior(a_post2, b_post2, "Posterior Distribution (Second Sample)")
print(plot2)