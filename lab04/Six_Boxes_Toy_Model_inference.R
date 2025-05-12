set.seed(42)  

black_counts <- c(5, 4, 3, 2, 1, 0)
total_balls <- 5
box_labels <- paste0("H", 0:5)

true_box <- sample(1:6, 1) 
cat("The true box is:", box_labels[true_box], "(hidden from model)\n\n")

# Prior probabilities (uniform)
priors <- rep(1/6, 6)

belief_history <- matrix(NA, nrow = 0, ncol = 6)
colnames(belief_history) <- box_labels

observations <- c()

# trials
num_trials <- 20
for (trial in 1:num_trials) {
  num_black <- black_counts[true_box]
  ball <- sample(c("black", "white"), size = 1, prob = c(num_black, total_balls - num_black) / total_balls)
  observations <- c(observations, ball)
  
  # Bayesian update
  likelihoods <- sapply(black_counts, function(b) {
    prob_black <- b / total_balls
    prod(ifelse(observations == "black", prob_black, 1 - prob_black))
  })
  
  # Bayes: posterior ∝ prior × likelihood
  unnorm_posteriors <- priors * likelihoods
  posteriors <- unnorm_posteriors / sum(unnorm_posteriors)

  cat(sprintf("Trial %d - Drew a %s ball\n", trial, ball))
  cat("Posterior probabilities:\n")
  for (i in 1:6) {
    cat(sprintf("%s: %.4f  ", box_labels[i], posteriors[i]))
  }
  cat("\n\n")

  belief_history <- rbind(belief_history, posteriors)
}

library(ggplot2)
library(reshape2)

belief_df <- as.data.frame(belief_history)
belief_df$Trial <- 1:nrow(belief_df)
belief_long <- melt(belief_df, id.vars = "Trial", variable.name = "Box", value.name = "Probability")

ggplot(belief_long, aes(x = Trial, y = Probability, color = Box)) +
  geom_line(size = 1.1) +
  labs(
    title = "Evolution of Posterior Probability for Each Box",
    x = "Trial Number",
    y = "Posterior Probability"
  ) +
  theme_minimal()
