
data {
  int<lower=1> N;
  vector[N] X;
  vector[N] Y;
  vector[N] sigma;
}
parameters {
  real beta0;
  real<lower=-1, upper=1> beta1;
  real beta2;
}
model {
  // Priors
  beta0 ~ normal(5, 1);
  beta2 ~ normal(30, 15);
  
  // Likelihood
  for (n in 1:N) {
    Y[n] ~ normal(beta0 + beta1 * X[n] + beta2 * square(X[n]), sigma[n]);
  }
}

