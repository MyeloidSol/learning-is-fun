library(ggplot2)
library(dagitty)
library(ggdag)
library(cmdstanr)

# Data Generating Process ----
g1 <- dagitty(
  "
  dag {
  X -> Y
  X -> Z -> Y
  }
  ")
ggdag(g1, layout = "circle") +
  theme_bw()

# Hyperparameters
n <- 1e6

# Variables
X <- rnorm(n, mean = 10, sd = 5)
Z <- rnorm(n, mean = 5 + 2*X, sd = 4) # Z = 5 + 2X
Y <- rnorm(n, mean = 1 + 4*X + 6*Z, sd = 3) # Y = 1 + 4X + 6Z

# Model Y with respect to Z
fit1 <- lm(Y ~ Z)
Y_res <- fit1$residuals

# Model the [Z]-residualized Y with respect to X
fit2 <- lm(Y_res ~ X)
print(fit2)

# Correct estimation
cfit <- lm(Y ~ X + Z)
print(cfit)

