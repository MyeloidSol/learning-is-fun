data {
  int<lower=1> Nobs; // number of observations
  
  vector<lower=0>[Nobs] word_freq; // word frequency
  vector<lower=0>[Nobs] complexity; // word complexity
}
parameters {
  real<lower=0> alpha;
  real<lower=0> delta;
  real<lower=0> beta;
  real<lower=0> kappa;
  
  real<lower=0> sigma; // dispersion term
}
model {
  vector[Nobs] mu;
  
  // Priors
  alpha ~ exponential(1e-2);
  delta ~ exponential(1e-2);
  
  beta ~ exponential(1e2); 
  kappa ~ exponential(1e-1);
  
  sigma ~ exponential(1); // large dispersion in complexity unlikely
  
  // Likelihood
  mu = log( (beta / kappa) * log(1 + exp(kappa * (word_freq - delta)) ) + alpha ) ;
  
  complexity ~ lognormal( mu, sigma );
}

