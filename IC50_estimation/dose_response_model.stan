data {
  // Number of biological replicates, technical replicates, and doses measured
  int<lower=1> Nb;
  int<lower=1> Nt;
  int<lower=1> Nd;
  
  vector<lower=0>[Nd] dose;
  array[Nd, Nt, Nb] real potency;
}
parameters {
  // Population of alpha across biological replicates
  real<lower=0> alphaP;
  real<lower=1e-15> dispA;
  
  // Population of beta across biological replicates
  real<lower=0> betaP;
  real<lower=1e-15> dispB;
  
  // Parameters for dose-response curve for each biological replicate
  vector<lower=0>[Nb] alphaI;
  vector<lower=0>[Nb] betaI;
  
  // Assay error
  real<lower=1e-15> error;
}
model {
  vector[Nd] mu;
  
  // Hyperpriors
  alphaP ~ lognormal(0, 1);
  dispA ~ exponential(1);
  
  betaP ~ lognormal(0, 1);
  dispB ~ exponential(1);
  
  // Priors
  alphaI ~ lognormal(log(alphaP), dispA);
  betaI ~ lognormal(log(betaP), dispB);
  
  // Likelihood
  for (i in 1:Nb) {
    for (j in 1:Nt) {
      mu = log(inv_logit(betaI[i] * dose - alphaI[i])); // Expected potency
      
      potency[,j,i] ~ lognormal(mu, error);
    }
  }
  
}


