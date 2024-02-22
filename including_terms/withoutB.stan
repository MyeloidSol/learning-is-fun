data {
  int<lower=1> Nobs;
  vector[Nobs] x;
  vector[Nobs] y;
}
parameters {
  real Au;
  real Ku;
  
  real sigmau;
}
transformed parameters {
  real A;
  real K;
  
  real sigma;
  
  A = exp(Au);
  K = exp(Ku);
  
  sigma = exp(sigmau);
}
model {
  
  y ~ normal(A * exp(-K * x), sigma);
  
}

