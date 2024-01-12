functions {
  // Calculate probabilities by splitting up normal distribution
  vector ord_probs(real mu, real sigma, vector c, int k) {
    vector[k] probs;
    
    // Initial category
    probs[1] = normal_cdf(c[1] | mu, sigma);
    
    // Interior categories(if present)
    if (k > 2) {
      for (i in 2:(k-1)) {
        probs[i] = normal_cdf(c[i] | mu, sigma) - normal_cdf(c[i - 1] | mu, sigma);
      }
    }

    // Final category
    probs[k] = 1 - normal_cdf(c[k - 1] | mu, sigma);
    
    return(probs);
  }
}

data {
  int<lower=1> Ncan; // Number of candidates
  int<lower=1> Nrev; // Number of reviewers
  int<lower=1> Nobs; // Number of nonmissing observations
  int<lower=1> Nk; // Number of discrete scores
  
  
  array[Nobs] int obs_scores; // Observed discrete scores
  array[Nobs] int can_ID; // Candidate ID
  array[Nobs] int rev_ID; // Reviewer ID
}

parameters {
  vector[Ncan] c_ability; // Candidate ability
  vector[Nrev] r_bias; // Reviewer bias
  
  real<lower=0> l_err; // Dispersion in observing a latent score
}

model {
  vector[Nk - 1] c = linspaced_vector(Nk - 1,0,Nk - 2);
  
  // Priors to constrain variables
  c_ability ~ normal(0, 1);
  r_bias ~ normal(0, 1);
  
  for (i in 1:Nobs) {
    real mu; // latent score
    mu = c_ability[can_ID[i]] + r_bias[rev_ID[i]];
    
    // Likelihood
    obs_scores[i] ~ categorical( ord_probs(mu, l_err, c, Nk) );
    
  }
}
