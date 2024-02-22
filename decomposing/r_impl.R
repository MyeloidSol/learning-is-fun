library(cmdstanr)
library(ggplot2)


# Data Generating Process ----
n <- 1e3
x <- seq(0, 200, length = n)

y <- rnorm(n, 
           mean = 5 + 0.1*x + 100*exp(- 1e-2* ((x - 50) ^2) ),
           sd = 1)

ggplot(data.frame(x=x,y=y), aes(x = x, y = y)) + 
  geom_point() + 
  theme_bw()



model <- cmdstan_model("/Users/todd/Code/learning-is-fun/decomposing/decomposition.stan")

fit <- model$sample(data = list(Nobs = n,
                                X = x,
                                Y = y),
                    chains = 4,
                    parallel_chains = 4)
