# Part a
set.seed(42)
n_people <- 1000
initial_knowers <- 5
n_round <- 15
n_simulations <- 1000

final_counts <- numeric(n_simulations)

for (sim in 1:n_simulations){
  rumor_status <- rep(0, n_people)
  knowers <- sample(1:n_people, initial_knowers)
  rumor_status[knowers] <- 1

  for (round in 1:n_round) {
    current_knowers <- which(rumor_status == 1)
    spread_targets <- sample(1:n_people, length(current_knowers), replace=TRUE)
    rumor_status[spread_targets] <- 1
  }
  final_counts[sim] <- sum(rumor_status) 
}
total_knowers <- sum(rumor_status)
cat("Number of people who know the rumor after 15 interactions:", total_knowers, "\n")

# Part b
mean_known <- mean(final_counts)

cat("Average number of people who know the rumor after 15 interactions:", mean_known, "\n")