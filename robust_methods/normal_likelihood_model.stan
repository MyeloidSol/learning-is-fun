data {
  int<lower=0> N;
  vector[N] x;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  // weak priors
  mu ~ normal(0, 50);
  sigma ~ exponential(0.1);
  
  x ~ normal(mu, sigma);
}
