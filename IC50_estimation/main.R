require(ggplot2)
require(reshape2)
require(cmdstanr)

# Custom Functions
inv_logit <- function(x) {
  return( 1 / (1 + exp(-x)) )
}

# Generating Test Data
Nb <- 5
Nt <- 5
Nd <- 20

# Distribution of true values generated from batch creation
true_sig_Apar <- rlnorm(Nb, mean = log(5), sd = 0.15) # No such thing as a negative concentration
true_sig_Bpar <- rlnorm(Nb, mean = log(1), sd = 0.15) # Constrained to be positive

# Concentrations to be measured
concs <- seq(0, 10, length = Nd)

data <- sapply(1:Nb, function(i) { # Biological Replicates
  sapply(1:Nt, function(j) { # Technical Replicates, evaluating the same concentrations multiple times
    
    # Assay error
    rlnorm(Nd, 
           mean = log(inv_logit(true_sig_Bpar[i] * concs - true_sig_Apar[i])), 
           sd = 0.03)
  })
})
dim(data) <- c(Nd,Nt,Nb) # Dosage, Technical Replicate, Biological Replicate
dimnames(data) <- list(concs,
                       1:Nt,
                       1:Nb)

# Reshape and format for ggplot use
plot_data <- reshape2::melt(data, value.name = "Potency")
colnames(plot_data) <- c("Dosage", "Technical_Replicate", "Biological_Replicate", "Potency")
plot_data$Technical_Replicate <- as.factor(plot_data$Technical_Replicate)
plot_data$Biological_Replicate <- as.factor(plot_data$Biological_Replicate)

# Plot
ggplot(plot_data, aes(x = Dosage, y = Potency, 
                      col = Biological_Replicate, shape = Technical_Replicate )) +
  geom_point() +
  ggtitle("Dose-Response Curve") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# Fit Model
model <- cmdstan_model("/Users/todd/Code/learning-is-fun/IC50_estimation/dose_response_model.stan")

# Format data as list for Stan
model_data <- list(Nb = Nb,
                   Nt = Nt,
                   Nd = Nd,
                   dose = concs,
                   potency = data)

fit <- model$sample(model_data, chains = 4, parallel_chains = 4)

draws <- as.data.frame(fit$draws(format = "df"))


# Estimated IC50s
est_IC50 <- colMeans(draws[,grepl("alphaI", colnames(draws))])
true_IC50 <- true_sig_Apar

# Compare
print(est_IC50)
print(true_IC50)


