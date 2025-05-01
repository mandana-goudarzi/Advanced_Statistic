# Part a
n <- 20 
r <- 12

#uniform prior
a_unif <- 1
b_unif <- 1

a_post_unif <- a_unif + r
b_post_unif <- b_unif + n - r

mean_unif <- a_post_unif/(a_post_unif + b_post_unif)
sd_unif <- sqrt((a_post_unif * b_post_unif) /
                  (((a_post_unif + b_post_unif)^2) *
                     (a_post_unif + b_post_unif + 1)))

#jeffrey prior
a_jeff <- 0.5
b_jeff <- 0.5

a_post_jeff <- a_jeff + r
b_post_jeff <- b_jeff + n - r

mean_jeff <- a_post_jeff/ (a_post_jeff + b_post_jeff)
sd_jeff <- sqrt((a_post_jeff * b_post_jeff) /
                     (((a_post_jeff + b_post_jeff)^2) *
                        (a_post_jeff + b_post_jeff + 1)))

cat("Uniform Prior (Beta(1,1)) Posterior: Beta(13,9)\n")
cat("  Mean:", mean_unif, "\n")
cat("  SD  :", sd_unif, "\n\n")

cat("Jeffrey's Prior (Beta(0.5,0.5)) Posterior: Beta(12.5,8.5)\n")
cat("  Mean:", mean_jeff, "\n")
cat("  SD  :", sd_jeff, "\n")

# Part b
mu_vals <- seq(0,1 , length.out=1000)

post_unif <- dbeta(mu_vals, shape1=a_post_unif, shape2=b_post_unif)
post_jeff <- dbeta(mu_vals, shape1=a_post_jeff, shape2=b_post_jeff)

plot(mu_vals, post_unif, type="l", lwd=2, ylab = "Density", xlab = expression(mu),
     main = "Posterior Distributions of μ", ylim = c(0, max(post_unif, post_jeff)))

lines(mu_vals, post_jeff, lwd = 2, col = "red", lty = 2)

legend("topright", legend = c("Uniform Prior (Beta(13,9))",
                              "Jeffrey's Prior (Beta(12.5,8.5))"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

#Part c
n_b <- 10
r_b <- 10

a_prior <- 1
b_prior <- 1

a_post_b <- a_prior + r_b
b_post_b <- b_prior + n_b - r_b

mean_b <- a_post_b / (a_post_b + b_post_b)
sd_b <- sqrt((a_post_b * b_post_b) /
               (((a_post_b + b_post_b)^2) * (a_post_b + b_post_b + 1)))

cat("Researcher B's Posterior (Uniform Prior → Beta(11,1))\n")
cat("  Mean:", mean_b, "\n")
cat("  Standard Deviation:", sd_b, "\n")

# Part d
a_prior_d <- 11
b_prior_d <- 1

a_post_d <- a_prior_d + r
b_post_d <- b_prior_d + n - r

mean_d <- a_post_d / (a_post_d + b_post_d)
sd_d <- sqrt((a_post_d * b_post_d) /
               (((a_post_d + b_post_d)^2) * (a_post_d + b_post_d + 1)))

cat("Updated Posterior Using Researcher B's Posterior as Prior:\n")
cat("  Posterior: Beta(23, 9)\n")
cat("  Mean:",mean_d, "\n")
cat("  Standard Deviation:",sd_d, "\n")

posterior_d <- dbeta(mu_vals, shape1 = a_post_d, shape2 = b_post_d)

plot(mu_vals, posterior_d, type = "l", lwd = 2, col = "purple",
     main = "Posterior of μ After Updating with Researcher A's Data",
     xlab = expression(mu), ylab = "Density")

# Part d
cred_int <- qbeta(c(0.025, 0.975), shape1=a_post_d, shape2=b_post_d)
cat("95% Credible Interval for μ (Posterior Beta(23, 9)):\n")
cat("  Lower bound:",cred_int[1], "\n")
cat("  Upper bound:",cred_int[2], "\n")
