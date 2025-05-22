set.seed(42)

n <- 100           
xmin <- 10         # min value
alpha <- 2.5       # power-law exponent

# Inverse transform sampling
# F(x) = 1 - (x_min / x)^(alpha - 1)
# x: x = xmin * (1 - U)^(-1 / (alpha - 1))

u <- runif(n)  # uniform random numbers between 0 and 1
x <- xmin * (1 - u)^(-1 / (alpha - 1))

head(x)
summary(x)

hist(x, breaks = 30, col = "skyblue", main = "Synthetic Power-Law Data", xlab = "x")