# Part a
n <- 28
r <- 13
p_vals <- seq(0,1, length.out = 1000)

prior_unif <- dbeta(p_vals, shape1= 1, shape2=1)
prior_beta <- dbeta(p_vals, shape1= 2, shape2=2)

likelihood <- dbinom(r, size=n, prob=p_vals)
likelihood <- likelihood/max(likelihood) #normalizing

post_unif <- dbeta(p_vals, shape1= 1 + r, shape2= 1 + n - r)
post_beta <- dbeta(p_vals, shape1= 2 + r, shape2= 2 + n - r)

#par(mfrow=c(2, 1))

#flat prior
plot(p_vals, prior_unif, type="l", col="blue", lwd=2, ylab = "Density", xlab = "p", main = "Flat Prior (Beta(1,1)) - Prior, Likelihood, Posterior")
lines(p_vals, likelihood, col= "orange", lwd=2)
lines(p_vals, post_unif, col= "darkgreen", lwd= 2)
legend("topright", legend=c("Prior", "Likelihood", "Posterior"), col=c("blue", "orange", "darkgreen"), lwd=2)
#informative prior
plot(p_vals, prior_beta, type="l", col="blue", lwd=2, ylab = "Density", xlab = "p", main = "Beta Prior (Beta(2,2)) - Prior, Likelihood, Posterior")
lines(p_vals, likelihood, col= "orange", lwd=2)
lines(p_vals, post_beta, col= "darkgreen", lwd=2)
legend("topright", legend = c("Prior", "Likelihood", "Posterior"), col=c("blue", "orange", "darkgreen"), lwd=2)

# Part b
mode_unif <- (1 + r - 1)/(1 + r + 1 + n - r - 2)
ci_unif <- qbeta(c(0.025, 0.975), shape1 = 1+r, shape2= 1+n-r)

mode_beta <- (2 + r - 1)/(2+r + 2+n-r - 2)
ci_beta <- qbeta(c(0.025, 0.975), shape1= 2+r, shape2= 2+n-r)

cat("Flat Prior (Beta(1,1)) → Posterior Beta(14,16)\n")
cat("  Mode (Most Probable p):", mode_unif,"\n")
cat("  95% Credible Interval:", ci_unif[1], ci_unif[2], "\n")

cat("Beta Prior (Beta(2,2)) → Posterior Beta(15,17)\n")
cat("  Mode (Most Probable p):", mode_beta, "\n")
cat("  95% Credible Interval:", ci_beta[1], ci_beta[2], "\n")

# Part c
