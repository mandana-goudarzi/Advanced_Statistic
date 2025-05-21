#part 1
g_target <- function(x) {
  phi0 <- 0.6
  phi1 <- 0.4
  mu <- c(-3, 3)
  sigma <- c(1, 1)
  phi0 * dnorm(x, mean = mu[1], sd = sigma[1]) +
  phi1 * dnorm(x, mean = mu[2], sd = sigma[2])
}

metropolis_hastings <- function(n_iter, init, proposal_sd) {
  samples <- numeric(n_iter)
  samples[1] <- init
  
  for (i in 2:n_iter) {
    current <- samples[i - 1]
    proposal <- rnorm(1, mean = current, sd = proposal_sd)  # proposal from N(current, 1)
    
    acceptance_ratio <- g_target(proposal) / g_target(current)
    
    if (runif(1) < acceptance_ratio) {
      samples[i] <- proposal
    } else {
      samples[i] <- current
    }
  }
  
  return(samples)
}
set.seed(42)
samples <- metropolis_hastings(n_iter = 10000, init = 0, proposal_sd = 1)

# histogram of the samples
hist(samples, breaks = 50, probability = TRUE, col = "lightblue",
     main = "Samples from Gaussian Mixture using Metropolis-Hastings", xlab = "x")

# overlay the true target distribution
curve(g_target, from = -8, to = 8, add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Target GMM", "Sampled"), col = c("red", "lightblue"), lwd = 2)

#analyzing mean and var
mean_value <- mean(samples)
variance_value <- var(samples)

cat("Estimated Mean:", mean_value, "\n")
cat("Estimated Variance:", variance_value, "\n")
library(coda)
mcmc_chain <- as.mcmc(samples)

summary(mcmc_chain)

#the trace and autocorrelation
plot(mcmc_chain)
acfplot(mcmc_chain)

burn_in <- 1000
thin_interval <- 10
samples_thinned <- samples[(burn_in + 1):length(samples)][seq(1, length(samples) - burn_in, by = thin_interval)]

# Reanalyze with CODA
mcmc_thinned <- as.mcmc(samples_thinned)
summary(mcmc_thinned)
plot(mcmc_thinned)
acfplot(mcmc_thinned)

par(mfrow = c(2, 1))
hist(samples, breaks = 50, col = "lightblue", probability = TRUE, main = "Original Chain")
hist(samples_thinned, breaks = 50, col = "orange", probability = TRUE, main = "After Burn-in + Thinning")

analyze_chain <- function(samples, burn_in = 0, thinning = 1, label = "") {
  #apply burn-in and thinning
  samples_processed <- samples[(burn_in + 1):length(samples)]
  samples_thinned <- samples_processed[seq(1, length(samples_processed), by = thinning)]
  
  # Convert to mcmc object
  mcmc_obj <- as.mcmc(samples_thinned)
  
  # histogram of posterior distribution
  hist(samples_thinned, breaks = 50, probability = TRUE, 
       main = paste("Posterior (Burn-in:", burn_in, ", Thinning:", thinning, ")", label),
       col = "lightblue", xlab = "x")
  curve(g_target, from = -8, to = 8, add = TRUE, col = "red", lwd = 2)
  
  # Plot ACF
  acf(samples_thinned, main = paste("ACF (Burn-in:", burn_in, ", Thinning:", thinning, ")", label))
  
  cat("\n--- Burn-in:", burn_in, "Thinning:", thinning, "---\n")
  print(summary(mcmc_obj))
  cat("\nAutocorrelation at lag 1:", autocorr(mcmc_obj)[1], "\n")
  
  return(list(samples = samples_thinned, mcmc = mcmc_obj))
}

par(mfrow = c(2, 2))  # For 2x2 plotting layout

# Try different configurations
res1 <- analyze_chain(samples, burn_in = 0, thinning = 1, label = "A")
res2 <- analyze_chain(samples, burn_in = 1000, thinning = 1, label = "B")
res3 <- analyze_chain(samples, burn_in = 1000, thinning = 5, label = "C")
res4 <- analyze_chain(samples, burn_in = 1000, thinning = 10, label = "D")