library(ggplot2)
library(cmdstanr)
library(tidyverse)


# Data Generating Process ---- 
Nobs <- 100
x <- seq(0.01, 100, length = Nobs)

# y ~ Aexp(-kt) + B
# Parameters per condition [Condition x Parameters]
pars <- tibble(
  cond = as_factor(1:8),
  A = rep(c(5,10), each = 4),
  K = rep(c(0.2,0.1), each = 4),
  B = rep(c(0, 0.1, 0.5, 1), times = 2)
)

# For each condition, generate data using the parameters
data <- pars %>% 
  mutate(vals = pmap(.l = ., .f = function(cond, A, K, B) {
    mu <- A * exp(-K * x) + B # Mu parameter
    
    y <- rnorm(Nobs, mu, 0.2) # y ~ Normal(mu, sigma)
    
    tibble(x = x, y = y) # Paired x and y values
  })) %>%
  select(cond, vals) %>%
  unnest(cols = c(vals))

# Visuals Part 1 ----
ggplot(data, aes(x = x, y = y, color = cond)) +
  geom_point() + 
  theme_classic() +
  labs(color = "Condition")


# The Models ----
## With B ----
model_withB <- cmdstan_model("/Users/todd/Code/learning-is-fun/including_terms/withB.stan")

post_wB <- lapply(levels(data$cond), function(cond_i) {
  d <- data %>% filter(cond == cond_i)
  
  model_data <- list(Nobs = Nobs,
                     x = d$x,
                     y = d$y)
  
  fit <- model_withB$sample(model_data, iter_warmup = 1500,
                      chains = 5, parallel_chains = 5,
                      init = function() {list(Au = 1, Ku = 1, Bu = 1, sigmau = 1)},
                      refresh = 1000)
  
  draws <- fit$draws(c("A", "K", "B"), format = "df") %>% 
    select(A, B, K) # Ignore warning
  
  draws
})

### Accuracy ----
acc_wB <- lapply(1:8, function(cond_i) {
  pars_i <- pars %>%
    filter(cond == cond_i)
  
  tibble(Aerr = mean((pars_i$A - post_wB[[cond_i]]$A)^2),
         Kerr = mean((pars_i$K - post_wB[[cond_i]]$K)^2))
})


## Without B ----
model_withoutB <- cmdstan_model("/Users/todd/Code/learning-is-fun/including_terms/withoutB.stan")

post_woB <- lapply(levels(data$cond), function(cond_i) {
  d <- data %>% filter(cond == cond_i)
  
  model_data <- list(Nobs = Nobs,
                     x = d$x,
                     y = d$y)
  
  fit <- model_withoutB$sample(model_data, iter_warmup = 1500,
                            chains = 5, parallel_chains = 5, 
                            init = function() {list(Au = 1, Ku = 1, sigmau = 1)},
                            refresh = 1000)
  
  draws <- fit$draws(c("A", "K"), format = "df") %>% 
    select(A, K) # Ignore warning
  
  draws
})

acc_woB <- lapply(1:8, function(cond_i) {
  pars_i <- pars %>%
    filter(cond == cond_i)
  
  tibble(Aerr = mean((pars_i$A - post_woB[[cond_i]]$A)^2),
         Kerr = mean((pars_i$K - post_woB[[cond_i]]$K)^2))
})

