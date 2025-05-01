# Part a
n <- 20 
r <- 12

#uniform prior
a_unif <- 1
b_unif <- 1

a_post_unif <- a_unif + r
b_post_unif <- b_unif + n - r

mean_unif <- a_post_unif/(a_post_unif + b_post_unif)
sd_unif <- sqrt((a_post_unif * b_post_unif) /
                  (((a_post_unif + b_post_unif)^2) *
                     (a_post_unif + b_post_unif + 1)))

#jeffrey prior
a_jeff <- 0.5
b_jeff <- 0.5

a_post_jeff <- a_jeff + r
b_post_jeff <- b_jeff + n - r

mean_jeff <- a_post_jeff/ (a_post_jeff + b_post_jeff)
sd_jeff <- sqrt((a_post_jeff * b_post_jeff) /
                     (((a_post_jeff + b_post_jeff)^2) *
                        (a_post_jeff + b_post_jeff + 1)))

cat("Uniform Prior (Beta(1,1)) Posterior: Beta(13,9)\n")
cat("  Mean:", mean_unif, "\n")
cat("  SD  :", sd_unif, "\n\n")

cat("Jeffrey's Prior (Beta(0.5,0.5)) Posterior: Beta(12.5,8.5)\n")
cat("  Mean:", mean_jeff, "\n")
cat("  SD  :", sd_jeff, "\n")