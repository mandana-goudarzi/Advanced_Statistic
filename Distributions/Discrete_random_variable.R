library(ggplot2)
# Question 1
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

# Question 2
# Plot of PMF
pmf_df <- data.frame(k = k_vals, PMF = pmf)
ggplot(pmf_df, aes(x = k, y = PMF)) + geom_bar(stat="identity") + labs(title = "Zero-Truncated Binomial PMF", x = "k", y = "P(X = k)") + theme_minimal()

# plot of cdf
cdf_df <- data.frame(k= k_vals, CDF = cdf)
ggplot(cdf_df, aes(x = k, y = CDF)) + geom_step() + labs(title = "Zero-Truncated Binomial CDF", x = "k", y = "P(X ≤ k)") + theme_minimal()

# Question 3
# mean
mean_ztb <- sum(k_vals * pmf)

# variance
var_ztb <- sum((k_vals^2) * pmf) - mean_ztb^2


cat("ZTB Mean:", mean_ztb, "\n")
cat("ZTB Variance:", var_ztb, "\n")

# standard binomial
mean_binom <- n * p
var_binom <- n * p * (1 - p)

p0 <- dbinom(0, size = n, prob =p)

mean_ztb_theoretical <- mean_binom/(1 - p0)
var_ztb_theoretical <- (var_binom / (1 - p0)) + (p0 / (1-p0)^2 * mean_binom^2)

cat("ZTB Analytical Mean:", mean_ztb_theoretical, "\n")
cat("ZTB Analytical Variance:", var_ztb_theoretical, "\n")

cat("Standard Binomial Mean:", mean_binom, "\n")
cat("Standard Binomial Variance:", var_binom, "\n")

# Question 4
sample_size <- 10000

set.seed(42)
x <- rbinom(2 * sample_size, size= n, prob= p )
ztb_sample <- x[x>0][1:sample_size]

hist(ztb_sample,breaks = seq(0.5, n + 0.5, 1), main= "Sample from Zero-Truncated Binomial Distribution", xlab= "Number of successes", ylab="Frequency")

sample_mean <- mean(ztb_sample)
cat("Sample Mean of ZTB:", sample_mean, "\n")
