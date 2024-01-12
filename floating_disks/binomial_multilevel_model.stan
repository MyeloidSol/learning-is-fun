data {
  int<lower=0> Nobs;
  int<lower=0> Nrep;
  
  array[Nobs, Nrep + 1] int<lower=0> mat; // First column is time, other columns are variables
}
parameters {
  real alphaP;
  real betaP;
  real<lower=1e-15> Adisp;
  real<lower=1e-15> Bdisp;
  
  vector[Nrep] c_alphaI;
  vector[Nrep] c_betaI;

}
transformed parameters{
  vector[Nrep] alphaI;
  vector[Nrep] betaI;
  
  // Non-centered parameters
  alphaI = c_alphaI * Adisp + alphaP;
  betaI = c_betaI * Bdisp + betaP;
}
model {
  real time;
  
  // Hyperpriors
  alphaP ~ normal(0, 10);
  betaP ~ normal(0, 10);
  Adisp ~ exponential(1);
  Bdisp ~ exponential(1);

  // Centered Priors
  c_alphaI ~ normal(0, 1);
  c_betaI ~ normal(0, 1);
  
  for (i in 1:3) { // Replicates
    for (j in 1:Nobs) { // Observations
      // Likelihood
      mat[j,i + 1] ~ binomial_logit(10, alphaI[i] + betaI[i] * mat[j, 1]  ); 
    }
  }
}

