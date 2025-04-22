library(ggplot2)
# Zero-Truncated Binomial PMF
dztbinom <- function(k,n,p) {
  if (any(k < 1 | k > n)) {
    return(rep(0, length(k)))
  }
  binom_prob <- dbinom(k, size=n, prob = p)
  trunc_factor <- 1 - (1 - p)^n
  return (binom_prob/trunc_factor)
}
# cumulative distribution (CDF)
pztbinom <- function(k,n,p) {
  sapply(k, function(x) sum(dztbinom(1:x, n, p)))
}

n <- 14
p <- 0.15
k_vals <- 1:n

pmf <- dztbinom(k_vals, n, p)
cdf <- pztbinom(k_vals, n, p)

print(pmf)
print(cdf)

# Plot of PMF
pmf_df <- data.frame(k = k_vals, PMF = pmf)
ggplot(pmf_df, aes(x = k, y = PMF)) + geom_bar(stat="identity") + labs(title = "Zero-Truncated Binomial PMF", x = "k", y = "P(X = k)") + theme_minimal()

# plot of cdf
cdf_df <- data.frame(k= k_vals, CDF = cdf)
ggplot(cdf_df, aes(x = k, y = CDF)) + geom_step() + labs(title = "Zero-Truncated Binomial CDF", x = "k", y = "P(X ≤ k)") + theme_minimal()