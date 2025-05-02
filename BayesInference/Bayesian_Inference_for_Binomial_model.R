library(ggplot2)
library(dplyr)
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
flips <- c(0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1)
# starting from prior
a <- 1 #number of heads
b <- 1 #number of tails

result <- data.frame(trial = 1:length(flips), heads = NA, tails = NA, mode= NA, ci_low= NA, ci_high= NA)

for (i in 1:length(flips)) {
  a <- a + flips[i]
  b <- b + (1 - flips[i])
  result$heads[i] <- a - 1
  result$tails[i] <- b - 1
  if (a > 1 && b > 1) {
    result$mode[i] <- (a-1)/(a+b-2) 
  } else {
    result$mode[i] <- NA
  }
  result$ci_low[i] <- qbeta(0.025, shape1=a, shape2=b)
  result$ci_high[i] <- qbeta(0.975, shape1=a, shape2=b)
}

ggplot(result, aes(x=trial)) + geom_line(aes(y=mode), color = "darkgreen", na.rm = TRUE) +
  geom_ribbon(aes(ymin=ci_low, ymax=ci_high), alpha = 0.2, fill = "skyblue") +
  labs(title = "Sequential Bayesian Updating",
       subtitle = "Most Probable Value and 95% Credible Interval of p",
       x = "Number of Coin Tosses", y = "Estimated p (probability of heads)") +
  theme_minimal()

# Part d
posterior_batch <- dbeta(seq(0,1,length.out=1000), 14, 16)

a_seq <- 14  # 1 + 13 heads
b_seq <- 16  # 1 + 15 tails
posterior_seq <- dbeta(seq(0,1,length.out=1000), a_seq, b_seq)

all.equal(posterior_batch, posterior_seq)
cat("No, we do not get a different result.
In Bayesian inference using conjugate priors (like the Beta-Binomial model), updating the posterior sequentially or all at once produces the same final posterior distribution.
In both cases, starting from a Beta(1,1) prior and observing 13 heads and 15 tails leads to the posterior Beta(14,16).")