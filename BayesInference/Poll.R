library(MCMCpack)

votes_current <- c(A = 57, B = 31, C = 45, D = 67)

prior_unif <- c(A=1, B=1, C=1, D=1)

post_unif <- prior_unif + votes_current

expected_unif <- post_unif / sum(post_unif) * 100

set.seed(123)
samples_unif <- rdirichlet(10000, post_unif)

ci_unif <- apply(samples_unif, 2, quantile, prob= c(0.16, 0.84)) * 100
colnames(ci_unif) <- names(expected_unif)
# Prior from previous poll
prior_poll <- c(A = 32, B = 14, C = 26, D = 28)
post_informed <- prior_poll + votes_current

expected_informed <- post_informed/sum(post_informed) * 100

set.seed(123)
samples_informed <- rdirichlet(10000, post_informed)

ci_informed <- apply(samples_informed, 2, quantile, prob = c(0.16, 0.84)) * 100
colnames(ci_informed) <- names(expected_informed)
cat("Results with Uniform Prior:\n")
for (party in names(expected_unif)) {
  cat(party, ": ",
      sprintf("%.2f", expected_unif[party]), "% (68%% CI: ",
      sprintf("%.2f", ci_unif[1, party]), "–", sprintf("%.2f", ci_unif[2, party]), "%)\n", sep = "")
}

cat("\nResults with Prior from Previous Poll:\n")
for (party in names(expected_informed)) {
  cat(party, ": ",
      sprintf("%.2f", expected_informed[party]), "% (68%% CI: ",
      sprintf("%.2f", ci_informed[1, party]), "–", sprintf("%.2f", ci_informed[2, party]), "%)\n", sep = "")
}