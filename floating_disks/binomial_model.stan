data {
  int<lower=0> Nobs;
  int<lower=0> Nrep;
  
  array[Nobs, Nrep + 1] int<lower=0> mat; // First column is time, other columns are variables
}
parameters {
  real alpha;
  real beta;
}
model {
  real time;

  alpha ~ normal(0, 100);
  beta ~ normal(0, 100);
  
  for (i in 1:3) { // Replicates
    for (j in 1:Nobs) { // Observations
      time = mat[j, 1] * 1.0;
      
      mat[j,i + 1] ~ binomial_logit(10, alpha + beta * time ); 
    }
  }
}

