library(ggplot2)
library(reshape2)
library(cmdstanr)

# Custom Functions
inv_logit <- function(x) {
  return( 1 / (1 + exp(-x)) )
}

# Data
data <- data.frame(time = 0:20,
                   b1 = c(rep(0,6), rep(1,2), 2, rep(3,2), rep(4,2), 7, rep(9,3), rep(10,4)),
                   b2 = c(rep(0,6), rep(1,3), rep(2,4), rep(3,2), 6, rep(9,2), rep(10,3)),
                   b3 = c(rep(0,7), 2, rep(3,3), 4, rep(6,2), 7, 8, rep(9,2), rep(10,3))
)

# Fit Model ----
model <- cmdstanr::cmdstan_model("/Users/todd/Code/learning-is-fun/floating_disks/binomial_model.stan")

model_data <- list(Nobs = 21,
                   Nrep = 3,
                   mat = as.matrix(data))
fit <- model$sample(data = model_data, iter_sampling = 2e3, iter_warmup = 1e3, chains = 8)
MAP_fit <- model$optimize(model_data)$mle()

draws <- fit$draws(format = "df")

# Plot Model ----
plot_data <- melt(data, id.vars = "time", variable.name = "beaker", value.name = "floatings")

# Posterior Predictive Distribution(for p and # of leaf discs)
x <- seq(0, 20, length= 2e3)

# Expected # of leaf discs
p_ppd <- apply(draws, 1, function(draw) { inv_logit(draw["alpha"] + draw["beta"] * x) })
p_CI <- apply(p_ppd, 1, function(draws) { quantile(draws, c(0.025, 0.975)) * 10 })

# Predicted # of leaf discs
y_ppd <- apply(p_ppd, 1, function(draws) { rbinom(2e4, 10, draws) })
y_CI <- apply(y_ppd, 2, function(draws) { quantile(draws, c(0.025, 0.975)) })

ggplot(plot_data, aes(x = time, y = floatings, col = beaker)) +
  geom_point() +
  geom_function(fun = function(x) { inv_logit(MAP_fit[1] + MAP_fit[2] * x) * 10},
                col = "black") +
  geom_ribbon(data = data.frame(x = x), # Credibility region for expected number
              aes(x = x,
                  ymin = p_CI[1,],
                  ymax = p_CI[2,], fill = "Population Mean # of Leaf Discs"), # fill gives identity to layer
              inherit.aes = F, alpha = 0.3) +
  geom_ribbon(data = data.frame(x = x), # Credibility region for predicted # of leaf discs
              aes(x = x,
                  ymin = y_CI[1,],
                  ymax = y_CI[2,], fill = "Predicted # of Leaf Discs"), # fill gives identity to layer
              inherit.aes = F, alpha = 0.3) +
  xlab("Time Elapsed (min)") +
  ylab("# of Buoyant Leaf Discs") +
  ggtitle("Effect of Light on Number of Buoyant Leaf Discs Over Time") +
  labs(col = "Beaker ID") +
  scale_fill_manual(name = "95% Compatability Intervals", values = c("Population Mean # of Leaf Discs" = "black", "Predicted # of Leaf Discs" = "gray")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



# Fit Multilevel Model ----
ml_model <- cmdstanr::cmdstan_model("/Users/todd/Code/learning-is-fun/floating_disks/binomial_multilevel_model.stan")

ml_fit <- ml_model$sample(data = model_data, iter_sampling = 1e3, iter_warmup = 1e3, chains = 4, parallel_chains = 4)
ml_MAP_fit <- ml_model$optimize(model_data)$mle()

print(ml_fit$summary()[-c(6:11),])

ml_draws <- ml_fit$draws(format = "df")


## Plot Multilevel Model ----
plot_data <- melt(data, id.vars = "time", variable.name = "beaker", value.name = "floatings")


### Posterior Predictive Distribution(for each beaker's p and overall # of leaf disks) ----
x <- seq(0, 20, length= 1e3)

# Expected # of leaf discs for each beaker
b1p_ppd <- apply(ml_draws, 1, function(draw) { inv_logit(draw["alphaI[1]"] + draw["betaI[1]"] * x) * 10 })
b2p_ppd <- apply(ml_draws, 1, function(draw) { inv_logit(draw["alphaI[2]"] + draw["betaI[2]"] * x) * 10 })
b3p_ppd <- apply(ml_draws, 1, function(draw) { inv_logit(draw["alphaI[3]"] + draw["betaI[3]"] * x) * 10 })

b1p_CI <- apply(b1p_ppd, 1, function(draws) { quantile(draws, c(0.05, 0.95)) })
b2p_CI <- apply(b2p_ppd, 1, function(draws) { quantile(draws, c(0.05, 0.95)) })
b3p_CI <- apply(b3p_ppd, 1, function(draws) { quantile(draws, c(0.05, 0.95)) })

# Predicted # of leaf discs for any beaker
bnewp_ppd <- apply(ml_draws, 1, function(draw) {
  alpha <- rnorm(1, draw["alphaP"], draw["Adisp"])
  beta <- rnorm(1, draw["betaP"], draw["Bdisp"])

  inv_logit(alpha + beta * x)
})

bnewy_ppd <- apply(bnewp_ppd, 1, function(draws) { rbinom(2e4, 10, draws) })
bnewy_CI <- apply(bnewy_ppd, 2, function(draws) { quantile(draws, c(0.05, 0.9)) })

# Create Plot
ggplot(plot_data, aes(x = time, y = floatings)) +
  geom_point(aes(col = beaker)) +
  scale_color_manual(values = c("#F8766D", "#00BA38", "#619CFF")) +
  labs(col = "Beaker ID") +
  ggnewscale::new_scale_color() +
  # MAP Estimates for Expected # of Leaf Discs
  geom_function(fun = function(i) { inv_logit(ml_MAP_fit["alphaI[1]"] + ml_MAP_fit["betaI[1]"] * i) * 10},
                mapping = aes(color = "Beaker 1"), linewidth = 0.8) +
  geom_function(fun = function(i) { inv_logit(ml_MAP_fit["alphaI[2]"] + ml_MAP_fit["betaI[2]"] * i) * 10},
                mapping = aes(color = "Beaker 2"), linewidth = 0.8) +
  geom_function(fun = function(i) { inv_logit(ml_MAP_fit["alphaI[3]"] + ml_MAP_fit["betaI[3]"] * i) * 10},
                mapping = aes(color = "Beaker 3"), linewidth = 0.8) +
  scale_color_manual(name = "MAP Estimates for Expected # of Leaf Discs",
                    values = c("Beaker 1" = "#F8766D",
                               "Beaker 2" = "#00BA38",
                               "Beaker 3" = "#619CFF")) +
  # Compatibility Regions for Expected # of Leaf Discs
  geom_ribbon(data = data.frame(x = x),
              aes(x = x,
                  ymin = b1p_CI[1,],
                  ymax = b1p_CI[2,],
                  fill = "Beaker 1"),
              inherit.aes = F, alpha = 0.2) +
  geom_ribbon(data = data.frame(x = x),
              aes(x = x,
                  ymin = b2p_CI[1,],
                  ymax = b2p_CI[2,],
                  fill = "Beaker 2"),
              inherit.aes = F, alpha = 0.2) +
  geom_ribbon(data = data.frame(x = x),
              aes(x = x,
                  ymin = b3p_CI[1,],
                  ymax = b3p_CI[2,],
                  fill = "Beaker 3"),
              inherit.aes = F, alpha = 0.2) +
  scale_fill_manual(name = "90% Compatability Intervals for Expected # of Leaf Discs",
                    values = c("Beaker 1" = "#F8766D",
                               "Beaker 2" = "#00BA38",
                               "Beaker 3" = "#619CFF")) +
  ggnewscale::new_scale_fill() +
  # Compatibility Region for Predicted # of Leaf Discs
  geom_ribbon(data = data.frame(x = x),
              aes(x = x,
              ymin = bnewy_CI[1,],
              ymax = bnewy_CI[2,],
              fill = "Hypothetical New Beaker"),
              inherit.aes = F, alpha = 0.25) +
  scale_fill_manual(name = "90% Compatability Interval for Predicted # of Leaf Discs",
                    values = c("Hypothetical New Beaker" = "grey")) +
  xlab("Time Elapsed (min)") +
  ylab("# of Buoyant Leaf Discs") +
  scale_y_continuous(breaks = 1:5*2) +
  ggtitle("Effect of Light on Number of Buoyant Leaf Discs Over Time") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

