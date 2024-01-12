require(dagitty)
require(ggplot2)

# First Example ----
g1 <- dagitty( "dag {
  X -> Y
  X1 <- X -> X2
  Y1 <- Y -> Y2
}")
plot(graphLayout(g1))

# Simulate data following the above DAG
n <- 5e4
X <- rnorm(n, mean = 0, sd = 1)
Y <- rnorm(n, mean = 3*X, sd = 0.5)

X1 <- rnorm(n, mean = 2*X, sd = 0.5)
X2 <- rnorm(n, mean = -3.5*X, sd = 0.5)

Y1 <- rnorm(n, mean = 1.5*Y, sd = 0.5)
Y2 <- rnorm(n, mean = 3*Y, sd = 0.5)

data <- rbind(X1,X2, Y1,Y2) # Columns are observations


# Approximate first canonical directions
iters <- sapply(1:1e4, function(i) {
  # Generate random canonical directions(weights) for both sets of variables
  wX <- rnorm(2); wX <- wX / sum(wX^2)^0.5
  wY <- rnorm(2); wY <- wY / sum(wY^2)^0.5
  
  # Produce first canonical variables
  ZX <- colSums(wX * data[1:2,])
  ZY <- colSums(wY * data[3:4,])
  
  # Correlation between canonical variables
  p <- cor(ZX, ZY)
  
  return( c(wX, wY, p) )
})

# First canonical directions
firstCD <- iters[1:4, which.max(iters[5,])]
names(firstCD) <- rownames(data)

ZX <- colSums(firstCD[1:2] * data[1:2,])
ZY <- colSums(firstCD[3:4] * data[3:4,])

message("First Canonical Directions:"); print(firstCD)
message("Correlation between First Canonical Variables: ", sprintf("%.4f", max(iters[5,]) ))
message("Correlation between X1 and Y1: ", sprintf("%.4f", cor(X1,Y1) ))
message("Correlation between Unobserved X and Y: ", sprintf("%.4f", cor(X,Y)))

plot(X, Y,
     main = "Relationship between X and Y")
plot(ZX, ZY,
     main = "Relationship between Canonical Variables ZX and ZY")
plot(X1, Y1,
     main = "Relationship between Descendants X1 and Y1")


# Second Example ----
g2 <- dagitty( "dag {
  X <- U -> Y
  X1 <- X -> X2
  Y1 <- Y -> Y2
}")
plot(graphLayout(g2))

# Simulate data following the above DAG
n <- 1e4
U <- rnorm(n, mean = 0, sd = 3)
X <- rnorm(n, mean = U, sd = 1)
Y <- rnorm(n, mean = U, sd = 0.5)

  
  
  