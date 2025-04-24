# Question a
E0 <- 7.25
gamma <- 2.7

f1 <- function(E) rep(1, length(E))
f2 <- function(E) (E - E0 + 1)^(-gamma)
int1 <- integrate(f1, lower = 0, upper = E0)$value
int2 <- integrate(f2 , lower=E0, upper=Inf)$value

total_area <- int1 + int2

N <- 1/total_area

cat("Normalization constant N =", N, "\n")

# Question b
p_E <- function(E) {ifelse (E < E0, N, N * (E - E0 + 1)^(-gamma))}

curve(p_E, from=0, to=100, n=1000, ylab = "p(E)", xlab = "Energy E (GeV)", main = "PDF of Cosmic Ray Muon Energy Distribution")

