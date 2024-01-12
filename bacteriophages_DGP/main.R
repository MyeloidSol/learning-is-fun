require(parallel)
require(dplyr)


# Generate Test Data ----
N <- rpois(1, lambda = 2e7) # Total number of bacteriophages produced

# Divide up work amongst cluster
cl <- makeCluster(detectCores() - 1)
clN <- round(N / length(cl)) 
clusterExport(cl, "clN")

# Bacteriophages inputted into experiment
data <- clusterEvalQ(cl, {
  # Count number of times unique bacteriophage pops up over clN iterations
  cl_input <- replicate(clN, paste(sample(letters[1:20], 5, T), collapse = "")) |> 
    table() |> # Frequency table
    as.data.frame()
  
  colnames(cl_input) <- c("ident", "iN")
  
  cl_input
})
stopCluster(cl)

# Merge data from different nodes
data <- bind_rows(data) |> 
  group_by(ident) |> # Merge rows by ident column
  summarise_all(sum) # Add iN column

# Simulate stochastic dissociation
data$counts <- rbinom(nrow(data), 
                       size = input$iN,
                       p = 0.1)

# Empirical PMF
sprintf("%.5f", (c(20^5 - nrow(data), table(data$iN)) / 20^5))

# True PMF(WIP)
pmf1 <- function(x, Ncomb, Nrep) {
  ( (1 / Ncomb) * (Nrep))^x * ( (Ncomb - 1) / Ncomb )^(Nrep) / factorial(x)
}

sprintf("%.5f", pmf(0:9, 20^5, N))


x <- replicate(1000, paste(sample(letters[1:2], 3, T), collapse = "")) |> 
  table() |> # Frequency table
  as.data.frame()

(c(2^3 - nrow(x), table(x$Freq)) / 2^3)
