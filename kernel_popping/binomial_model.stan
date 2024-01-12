data {
  int<lower=1> Nobs; // Number of observations
  vector[Nobs] time; // Recorded time
}
parameters {
  real<lower=0> shape;
  real<lower=0> rate;
}
model {
  time ~ gamma(shape, rate);
}
