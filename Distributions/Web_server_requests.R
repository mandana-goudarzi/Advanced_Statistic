# Part a
mean_request <- 7
threshold <- 30

markov_bound <- mean_request/threshold
cat("Markov's inequality upper bound for P(X ≥ 30):", markov_bound, "\n")

# Part b
lambda <- 7
prob_at_least_30 <- 1 - ppois(29, lambda=lambda)

cat("Exact probability P(X ≥ 30) with Poisson(7):", prob_at_least_30, "\n")