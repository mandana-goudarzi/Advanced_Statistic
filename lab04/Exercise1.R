# Part a
n <- 75 #number of patients
p <- 0.15 #prob of failure old method
y <- 0:n #possible number of failure
pmf <- dbinom(y, size=n, prob=p )
plot(y, pmf, type="h", lwd = 2, col = "steelblue",
     main = "Binomial PMF: False Negatives (p = 0.15)",
     xlab = "Number of False Negatives (y)",
     ylab = "Probability")

#part b
y <- 6
p_hat <- y/n
cat("Estimated failure probability (frequentist):", p_hat, "\n")

