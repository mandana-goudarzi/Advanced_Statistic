# Part a
mean_request <- 7
threshold <- 30

markov_bound <- mean_request/threshold
cat("Markov's inequality upper bound for P(X ≥ 30):", markov_bound, "\n")
