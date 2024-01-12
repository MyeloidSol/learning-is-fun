library(tidyverse)
library(cmdstanr)

# Functions ----
# Masses of a normal distribution cut up into k - 1 pieces
ord_probs <- function(cuts = 1:(Nk-1) - Nk/2, mean, sd = 1) {
  
  probs <- c(pnorm(cuts, mean, sd),1)
  
  # -Inf --P[1]-- c[1] --P[2]-- c[2] --P[k-1]-- c[k-1] --P[k]-- +Inf
  probs <- c(probs[1], diff(probs))
}


# Generate Test Data ----
Ncan <- 15 # Number of candidates
Nrev <- 15 # Number of reviewers
Nsco <- 15 # Number of scores per candidate
Nk <- 5 # Number of discrete scores
err <- 0.25 # Error in latent score

# Variables of interest
candidate_ability <- rnorm(Ncan)
reviewer_bias <- rnorm(Nrev)

# Unobserved continuous score = [candidate_ability] + [reviewer_bias]
latent_scores <- outer(candidate_ability, reviewer_bias, FUN = "+")
latent_scores[] <- rnorm(Ncan*Nrev, mean = latent_scores, sd = err) # ~ error in latent score

# Each candidate is only scored by [Nsco] reviewers
latent_scores <- apply(latent_scores, 1, function(can) {
  can[sample(1:Nrev, Nrev - Nsco)] <- NA # Omit all but [Nsco] scores
  
  can
}) |> t()

# Generate discrete measured scores
obs_scores <- apply(latent_scores, 1, function(can) {
  
  obs <- sapply(can, function(l_score) {
    
    # Discretize latent scale into [Nk] categories
    cuts <- (1:(Nk-1) - Nk/2) * sqrt(2)
    
    # Assign discrete score, 5 being best, 1 being worst
    o_score <- cut(l_score, 
                   breaks = c(-Inf, cuts, Inf), 
                   labels = c(1:Nk))
    
    o_score
  })
  
  obs
}) |> t()


# Reformat missing scores for Stan
obs_scores <- as_tibble(obs_scores, rownames = "candidate_ID")
colnames(obs_scores)[-1] <- 1:Nrev

# Convert to long format
obs_scores <- obs_scores |>
  gather(key = "reviewer_ID", 
         value = "score", 
         -candidate_ID) |>
  filter(!is.na(score)) |>
  mutate_all(as.numeric)

# Modelling ----
model <- cmdstan_model("/Users/todd/Code/Learning/candidate_ranking/ordinal_model.stan")

data <- list(Ncan = Ncan,
             Nrev = Nrev,
             Nobs = nrow(obs_scores),
             Nk = Nk,
             obs_scores = obs_scores$score,
             can_ID = obs_scores$candidate_ID,
             rev_ID = obs_scores$reviewer_ID)


fit <- model$sample(data = data, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000)

draws <- fit$draws(format="df")
