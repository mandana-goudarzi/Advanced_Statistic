# Part a
lambda <- 2

prob_6 <- 1 - ppois(6, lambda=lambda * 2)

cat("P(more than 6 passengers after 2 minutes):", prob_6, "\n")

prob_4 <- ppois(4, lambda=lambda * 3)

cat("P(less than 4 passengers after 3 minutes):", prob_4, "\n")

# Part b
k <- 3
n_sim <- 10000

arrival_times <- rgamma(n_sim, shape=k, rate=lambda)

hist(arrival_times, breaks=50, probability=TRUE,main = "Arrival Time of 3rd Passenger",xlab = "Time (minutes)")

curve(dgamma(x, shape=k, rate=lambda),col="red", lwd = 2, add=TRUE)

# Part c
K <- 4

time_diff <- rgamma(n_sim, shape=K, rate=lambda)

hist(time_diff, breaks=50, probability=TRUE,main = "Time Difference: 5th - 1st Passenger",xlab = "Time (minutes)")

curve(dgamma(x, shape=K, rate=lambda),col="red", lwd = 2, add=TRUE)