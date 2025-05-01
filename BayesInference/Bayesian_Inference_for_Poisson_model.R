# Part a

x <- c(3, 0, 1, 5, 2)
n <- length(x) # number of observations (5 days)
sum_x <- sum(x) # total number of defects

alpha <- sum_x + 1
beta <- n

mu_vals <- seq(0, 10, length.out = 1000)
posterior_vals <- dgamma(mu_vals, shape = alpha, rate = beta)

plot(mu_vals, posterior_vals, type = "l", lwd = 2, main = "Posterior Distribution of μ", xlab = expression(mu), ylab="Density")

#analytical
mean_post <- alpha/beta
var_post <- alpha/ (beta^2)
median_post <- qgamma(0.5, shape=alpha,rate=beta)

cat("Analytical Posterior Mean:", mean_post, "\n")
cat("Analytical Posterior Median:", median_post, "\n")
cat("Analytical Posterior Variance:", var_post, "\n")

#numerical
set.seed(42)
posterior_samples <- rgamma(10000, shape=alpha, rate=beta)

mean_num <- mean(posterior_samples)
median_num <- median(posterior_samples)
var_num <- var(posterior_samples)

cat("\nNumerical Posterior Mean:", mean_num, "\n")
cat("Numerical Posterior Median:", median_num, "\n")
cat("Numerical Posterior Variance:", var_num, "\n")

# Part b
alpha_prior <- 4
beta_prior <- 2

alpha_post <- alpha_prior + sum_x
beta_post <- beta_prior + n

posterior_vals <- dgamma(mu_vals, shape= alpha_post, rate=beta_post)

plot(mu_vals, posterior_vals, type="l" ,lwd = 2, main = "Posterior Distribution of μ (Gamma Prior)", xlab = expression(mu), ylab="Density" )

#analytical
mean_poster <- alpha_post/ beta_post
var_poster <- alpha_post/(beta_post^2)
median_poster <- qgamma(0.5, shape=alpha_post, rate=beta_post)

cat("Analytical Posterior Mean:", mean_poster, "\n")
cat("Analytical Posterior Median:", median_poster, "\n")
cat("Analytical Posterior Variance:", var_poster, "\n")

#numerica
set.seed(42)
poster_samples <-rgamma(10000, shape=alpha_post, rate=beta_post)

mean <- mean(poster_samples)
median <- median(poster_samples)
var <- var(poster_samples)

cat("\nNumerical Posterior Mean:", mean, "\n")
cat("Numerical Posterior Median:", median, "\n")
cat("Numerical Posterior Variance:", var, "\n")

# part c
sd_unif <- sqrt(alpha/(beta^2))

ci_unif <- qgamma(c(0.025, 0.975), shape=alpha, rate=beta)

#normal approx using same mean and sd
ci_unif_norm <- mean_post + qnorm(c(0.025, 0.975)) * sd_unif

sd_gamma <- sqrt(alpha_post/(beta_post^2))

ci_gamma <- qgamma(c(0.025, 0.975), shape= alpha_post, rate=beta_post)
ci_gamma_norm <- mean_poster + qnorm(c(0.025, 0.975)) * sd_gamma

cat("Uniform prior posterior (Gamma(12, 5)):\n")
cat("95% credibility interval (Gamma):", ci_unif, "\n")
cat("95% credibility interval (normal approx):", ci_unif_norm, "\n\n")
cat("Informative Gamma prior posterior (Gamma(15, 7)):\n")
cat("95% credibility interval (Gamma):", ci_gamma, "\n")
cat("95% credibility interval (normal approx):", ci_gamma_norm)