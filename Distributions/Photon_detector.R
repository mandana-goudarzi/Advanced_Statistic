# Part a
mean_sec = 350
sd_sec = 75
target_signal = 1e6
z_val <- qnorm(0.05)

required_time <- function(T_sec) {
  mu <- mean_sec * T_sec
  sigma <- sd_sec *sqrt(T_sec)
  z <- (target_signal - mu)/sigma
  return (z)
}

T_guess <- 1e4
while(required_time(T_guess) > z_val){
  T_guess <- T_guess + 1
}

T_min <- T_guess / 60

cat("Minimum time to run the detector:", T_min, "minutes\n")