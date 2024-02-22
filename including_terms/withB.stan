data {
  int<lower=1> Nobs;
  vector[Nobs] x;
  vector[Nobs] y;
}
parameters {
  real Au;
  real Ku;
  real Bu;
  
  real sigmau;
}
transformed parameters {
  real A;
  real K;
  real B;
  
  real sigma;
  
  A = exp(Au);
  K = exp(Ku);
  B = exp(Bu);
  
  sigma = exp(sigmau);
}
model {
  
  y ~ normal(A * exp(-K * x) + B, sigma);
  
}

