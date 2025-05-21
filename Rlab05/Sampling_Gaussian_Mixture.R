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
