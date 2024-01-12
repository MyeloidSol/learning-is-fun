library(cmdstanr)
library(rstan)
library(bridgesampling)

# Test Data
data <- list(N = 100,
             x = rnorm(100, 10, 1))

# cmdstanr model ----
cmd_code <- cmdstanr::write_stan_file("
data {
  int N;
  vector[N] x;
}
parameters {
  real mu;
}
model {
  // Prior
  mu ~ normal(0, 100);
  
  // Likelihood
  x ~ normal(mu, 1);
}

")

cmd_model <- cmdstanr::cmdstan_model(cmd_code)

# sample posterior
cmd_fit <- cmd_model$sample(data = data,
                            iter_warmup = 1000, iter_sampling = 5000,
                            chains = 6, parallel_chains = 6)

# grab draws
cmd_draws <- cmd_fit$draws(format = "df")


# plot unstandardized posterior
i <- sample(1:nrow(cmd_draws), 10000) # subset points so graphics don't take forever
plot(x = cmd_draws$mu[i], y = exp(cmd_draws$lp__[i]), 
     main = "Unnormalized Posterior", 
     xlab = "Parameter", ylab = "Unnormalized Posterior Density",
     cex = 0.2)

# KDE posterior
kde_posterior <- density(cmd_draws$mu)

# plot posterior
plot(kde_posterior, main = "Kernel Density Estimated Posterior", 
     xlab = "Parameter", ylab = "Normalized Posterior Density") 

# estimate posterior density for each draw
post_prob_dens <- approx(kde_posterior$x, kde_posterior$y, xout = cmd_draws$mu)$y

# unstandardized posterior density for each draw
unnorm_dens <- exp(cmd_draws$lp__)

# unnorm. density / posterior probability density = marginal likelihood
cmd_ml_ests <- unnorm_dens / post_prob_dens


# rstan model ----
rstan_model <- stan_model(model_code = "
data {
  int N;
  vector[N] x;
}
parameters {
  real mu;
}
model {
  // Prior
  mu ~ normal(0, 100);
  
  // Likelihood
  x ~ normal(mu, 1);
}

")

# sample
rstan_fit <- rstan::sampling(rstan_model, data = data, 
                             iter = 5000 + 1000, warmup = 1000,
                             chains = 12, cores = 4)

# compute marginal likelihood
rstan_ml_est <- median(bridge_sampler(rstan_fit, silent = T, repetitions = 32, cores = 6)$logml)

# plot estimates for the marginal likelihood
LB <- mean(cmd_ml_ests) - 4 * sd(cmd_ml_ests)
UB <- mean(cmd_ml_ests) + 4 * sd(cmd_ml_ests)

plot(density(cmd_ml_ests, from = LB, to = UB),
     main = "Marginal Likelihood Estimates", col = "blue")
abline(v = exp(rstan_ml_est), 
       lty = "dashed", col = "red")
abline(v = mean(cmd_ml_ests), 
       lty = "dotted", col = "blue")
legend("topleft", 
       legend = c("Unnormalized / Kernel Estimates",
                  "(My Method) Mean Point-Estimate",
                  "Bridgesampling Point-Estimate"),
       col = c("blue", "blue", "red"), lty = c("solid", "dotted", "dashed"), 
       cex = 0.8)

# plot estimates for the log marginal likelihood
LB <- mean(log(cmd_ml_ests)) - 4 * sd(log(cmd_ml_ests))
UB <- mean(log(cmd_ml_ests)) + 4 * sd(log(cmd_ml_ests))

plot(density(log(cmd_ml_ests),from = LB, to = UB), 
     main = "Log Marginal Likelihood Estimates", col = "blue")
abline(v = rstan_ml_est, 
       lty = "dashed", col = "red")
abline(v = log(mean(cmd_ml_ests)), 
       lty = "dotted", col = "blue")
legend("topleft", 
       legend = c("Unnormalized / Kernel Estimates",
                  "(My Method) Mean Point-Estimate",
                  "Bridgesampling Point-Estimate"),
       col = c("blue", "blue", "red"), lty = c("solid", "dotted", "dashed"), 
       cex = 0.8)


# plot posteriors
plot(x = cmd_draws$mu[i], y = (unnorm_dens[i] / median(cmd_ml_ests)), 
     main = "Posterior Distribution",
     xlab = "Parameter", ylab = "Normalized Posterior Density",
     col = "red", cex = 0.1)
lines(kde_posterior,
      lty = "dashed", col = "blue")
legend("topleft", 
       legend = c("Normalized(with ML Estimate) Posterior", "Kernel Density Estimated Posterior"),
       col = c("red", "blue"), lty = c("solid", "dashed"), 
       cex = 0.7)


# method 2 ----
error <- sapply(1:nrow(cmd_draws), function(i) {
  true_ratios <- unnorm_dens[i] / unnorm_dens
  
  est_ratios <- post_prob_dens[i] / post_prob_dens
  
  avg_error <- mean(abs(true_ratios - est_ratios))
})


plot(x = error, y = cmd_ml_ests,
     main = "Dependence of ML Estimate on Average Error in Density Ratios",
     xlab = "Error", ylab = "Marginal Likelihood Estimate",
     col = "blue",
     cex = 0.3)
abline(h = exp(rstan_ml_est), 
       lty = "dashed", col = "red")
legend("topright",
       legend = c("CmdStanR (My Method) Estimates", "RStan Bridgesampling Estimate"),
       lty = c("solid", "dashed"),
       col = c("blue", "red")
                  )


