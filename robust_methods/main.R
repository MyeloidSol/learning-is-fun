require(ggplot2)
require(cmdstanr)

# Simulate Data
true_mu <- 10
true_sd <- 1
x <- rnorm(10, mean = true_mu, sd = true_sd)

data <- list(N = length(x),
             x = x)

# Fit Model 1(Normal Likelihood)
n_model <- cmdstanr::cmdstan_model("/Users/todd/Code/learning-is-fun/robust_methods/normal_likelihood_model.stan")

n_fit <- n_model$sample(data = data, chains = 4, parallel_chains = 4, iter_warmup = 1e3, iter_sampling = 1e4)

n_draws <- n_fit$draws(format = "df")

n_macc <- mean((n_draws$mu - true_mu)^2)
n_sacc<- mean((n_draws$sigma - true_sd)^2)


# Fit Model 2(T-likelihood)
t_model <- cmdstanr::cmdstan_model("/Users/todd/Code/learning-is-fun/robust_methods/t_likelihood_model.stan")

t_fit <- t_model$sample(data = data, chains = 4, parallel_chains = 4, iter_warmup = 1e3, iter_sampling = 1e4)

t_draws <- t_fit$draws(format = "df")

t_macc <- mean((t_draws$mu - true_mu)^2)
t_sacc<- mean((t_draws$sigma - true_sd)^2)


iters <- sapply(1:100, function(i) {
  # Simulate Data
  true_mu <- 10
  true_sd <- 1
  
  x <- rnorm(10, mean = true_mu, sd = true_sd)
  
  data <- list(N = length(x),
               x = x)
  
  
  # Fit Model 1(Normal Likelihood)
  n_fit <- n_model$sample(data = data, chains = 4, parallel_chains = 4, iter_warmup = 500, iter_sampling = 2e3, refresh = 2e3)
  
  n_draws <- n_fit$draws(format = "df")
  
  n_macc <- mean((n_draws$mu - true_mu)^2)
  n_sacc<- mean((n_draws$sigma - true_sd)^2)
  
  
  # Fit Model 2(T-likelihood)
  t_fit <- t_model$sample(data = data, chains = 4, parallel_chains = 4, iter_warmup = 500, iter_sampling = 2e3, refresh = 2e3)
  
  t_draws <- t_fit$draws(format = "df")
  
  t_macc <- mean((t_draws$mu - true_mu)^2)
  t_sacc<- mean((t_draws$sigma - true_sd)^2)
  
  
  return(c(n_macc, t_macc))
})
  