# Packages ----
library(ggplot2)
library(tidyverse)


# Test Data ----
n_provs <- 3
n_seas <- 10
n_obs <- 100

# For a given province, each province's parameters is drawn from a normal
# Not that important to include into the model
prov_pars <- tibble(alpha = rnorm(n_provs, mean = 5, sd = 1),
                    beta = rexp(n_provs, 1 / 2) + 1,
                    prov = 1:n_provs)

# For a given province, each season's parameters is drawn from a normal
seas_pars <- apply(prov_pars, 1, function(pars) {
  tibble(season = 1:n_seas,
         alpha = rnorm(n_seas, mean = pars["alpha"], sd = 0.25), # higher sd --> more variation between seasons
         beta = rnorm(n_seas, mean = pars["beta"], sd = 0.25) # higher sd --> more variation between seasons
         )
})

# Final parameter matrix
total_pars <- bind_rows(seas_pars, .id = "province") %>% 
  mutate_at(.vars = c("alpha", "beta"), .funs = as.numeric)


# Observed data
data <- apply(total_pars, 1, function(pars) {
  
  test_x <- rexp(n_obs, 1 / 10)
  season_data <- tibble(province = rep(pars["province"], n_obs),
                        season = rep(pars["season"], n_obs),
                        test_x = test_x,
                        test_y = rnorm(n_obs, 
                                       mean = as.numeric(pars["alpha"]) + as.numeric(pars["beta"]) * test_x, # best proxy
                                       sd = 0.5) # higher sd --> worse proxy
                        )
}) %>% bind_rows() %>% mutate_at(c("season"), as.integer)

# Plot
ggplot(data[data$province == 1 & data$season < 5,], aes(x = test_x, y = test_y, color = as.factor(season))) +
  geom_point() + 
  xlab("Positive Tests") +
  ylab("Hospitalizations") +
  labs(color = "Season") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid = element_blank(), axis.line = element_line(colour = "black"))

