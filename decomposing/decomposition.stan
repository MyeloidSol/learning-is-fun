data {
  int<lower=1> Nobs;
  
  array[Nobs] int<lower=0> Y;
  vector[Nobs] X;
}
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  
  real<lower=0> mu;
  real<lower = 0> disp;
  real<lower=0> scale;
  
  real<lower=0> sigma;
  
}
model {
  real trend;
  real peak;
  
  for (i in 1:Nobs) {
    trend = alpha + beta * X[i];
    peak = scale * exp( -(1/disp) * (X[i] - mu)^2 );
    print("Xvalue: ", X[i]);
    print("Trend: ", trend);
    print("Peak: ", peak);
    
    Y[i] ~ normal(trend + peak, sigma);
  }

}

