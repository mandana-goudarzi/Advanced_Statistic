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