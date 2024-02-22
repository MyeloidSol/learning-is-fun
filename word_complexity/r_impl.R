# Packages ----
library(ggplot2)
library(cmdstanr)


# Test Data Generation ----
Nobs <- 1e3

# True Parameters
t_alpha <- 30
t_beta <- 0
t_sigma <- 0.25

# Test Data
word_freq <- rlnorm(Nobs, 1.5, 2.5)
complexity <- rlnorm(Nobs, 
                     meanlog = log(t_alpha + t_beta * word_freq),
                     sdlog = t_sigma)

data <- data.frame(word_freq = word_freq,
                   complexity = complexity)

# Your Plot
plot(word_freq, complexity)


# The Model ----
model <- cmdstanr::cmdstan_model("/Users/todd/Code/learning-is-fun/word_complexity/lognormal_model.stan")

# Format for Stan
model_data <- list(Nobs = length(complexity),
                   word_freq = word_freq,
                   complexity = complexity)

# Fitting Model
fit <- model$sample(model_data, chains = 4, parallel_chains = 4)

# Posterior Draws
draws <- fit$draws(format = "df")
pairs(draws[,2:4])


# Plotting Model ----

# Predictive Posterior Distributions
exc <- diff(range(word_freq)) / sqrt(diff(range(word_freq)))
bounds <- c(min(word_freq) - exc, max(word_freq) + exc)

x <- seq(bounds[1], bounds[2], length = 2000)

# posterior for the expectation for a given frequency = x [X Value * Posterior Draw]
ppd_ex <- apply(draws, 1, function(draw) { exp(draw["alpha"] + draw["beta"] * x + draw["sigma"] / 2) })
ex_CI <- apply(ppd_ex, 1, function(draws) { quantile(draws, c(0.025, 0.975)) })

# posterior mean Estimate for `mu`
pme_mu <- function(freq) { exp(mean(draws$alpha) + mean(draws$beta) * freq + mean(draws$sigma) / 2) }

# posterior for observed complexity for a given frequency = x
#ppd_comp <- apply(draws, 1, 
#                  function(draw) { rlnorm(1e4, 
#                                          meanlog = draw["alpha"] + draw["beta"] * x,
#                                          sdlog = draw["sigma"])
#})


# Plot
ggplot(data, aes(x = word_freq, y = complexity)) +
  geom_point() + 
  xlab("Word Frequency") + 
  ylab("Word Complexity") + 
  ggtitle("Relationship between Word Complexity and Frequency") + 
  geom_function(fun = pme_mu, aes(color = "Expectation of Complexity")) + 
  geom_ribbon(data = data.frame(x = x), # Credibility region for expectation
              aes(x = x,
                  ymin = ex_CI[1,],
                  ymax = ex_CI[2,], fill = "Expectation of Complexity"),
              inherit.aes = F,
              alpha = 0.3) +
  scale_fill_manual(name = "95% Compatability Intervals", values = c("Expectation of Complexity" = "black")) +
  scale_color_manual(name = "Posterior Mean Estimate", values = c("Expectation of Complexity" = "black")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
