# Required Packages
require(ggplot2)

# Generate data
N <- 1e7
V1 <- rnorm(N, 15, sd = 2)
V2 <- 5 + rnorm(N, 2*V1, sd = 0.5)


# Format as a matrix
mat <- rbind(V1, V2)

# Distance matrix(thing we want to approximate)
dist_mat <- dist(t(mat))

# An orthogonal projection is the closest point on a line to a datapoint
# When both the line and datapoint are represented as vectors, the formula is:
# projected_point_vec = sum(point_vec * line_vec) / sum(line_vec * line_vec) * line_vec
# We then ask where along the line is the projected point,
# This is essentially "how many line_vec's go into projected_point_vec"
# Which is projected_point_vec / line_vec, and we are left with:
# projected_point = sum(point_vec * line_vec) / sum(line_vec * line_vec)
orthogonal_projection <- function(set_of_vecs, vec) {
  return( colSums(set_of_vecs * vec) )
}

# We will generate many random vectors to approximate the first PC
iters <- sapply(1:1e3, function(i) {
  # Generate random vector
  vec <- rnorm(2)
  vec <- vec / sum(vec^2)^0.5

  proj <- orthogonal_projection(mat, vec)

  # Calculate variance of data projected onto data
  proj_var <- var(proj)
  proj_mdist <- sum( (dist_mat - dist(proj))^2 )^0.5

  return(c(vec, proj_var, proj_mdist))
})

firstPC <- iters[1:2, which.max(iters[3,])]

# Plot data ----
plot_data <- data.frame(mat[1,], mat[2,])

plot <- ggplot(data = plot_data,
               mapping = aes(x = V1, y = V2)) +
  ggtitle("Cloud of Data and First Principal Component") +
  coord_cartesian(xlim = c(min(0, firstPC[1], plot_data$V1), max(0, firstPC[1], plot_data$V1)),
                  ylim = c(min(0, firstPC[2], plot_data$V2), max(0, firstPC[2], plot_data$V2)),
                  clip = "off") +
  geom_point() +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

# Plot first principal component
plot <- plot + geom_segment(aes(x = -firstPC[1]*1e3, y = -firstPC[2]*1e3,
                                xend = firstPC[1]*1e3, yend = firstPC[2]*1e3),
                            colour = "green", lwd = 0.25)

plot <- plot + geom_segment(aes(x = 0, y = 0,
                                xend = firstPC[1], yend = firstPC[2]),
                            arrow = arrow(length = unit(0.25, "cm")),
                            colour = "red", lwd = 1)

print(plot)



# Plot projected data ----
plot_data <- data.frame(outer(orthogonal_projection(mat, firstPC), firstPC))
colnames(plot_data) <- c("V1", "V2")

plot <- ggplot(data = plot_data,
               mapping = aes(x = V1, y = V2)) +
  ggtitle("Projected Data") +
  coord_cartesian(xlim = c(min(0, firstPC[1]*5, plot_data$V1), max(0, firstPC[1]*5, plot_data$V1)),
                  ylim = c(min(0, firstPC[2]*5, plot_data$V2), max(0, firstPC[2]*5, plot_data$V2)),
                  clip = "off") +
  geom_point() +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

# Plot first principal component
plot <- plot + geom_segment(aes(x = -firstPC[1]*1e3, y = -firstPC[2]*1e3,
                                xend = firstPC[1]*1e3, yend = firstPC[2]*1e3),
                            colour = "green", lwd = 0.25)

plot <- plot + geom_segment(aes(x = 0, y = 0,
                                xend = firstPC[1], yend = firstPC[2]),
                            arrow = arrow(length = unit(0.25, "cm")),
                            colour = "red", lwd = 1)

print(plot)

# Colour gradient function
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

# Plot parameter space(for projected variance and mean error) ----
plot_data <- data.frame(V1 = iters[1,], V2 = iters[2,],
                        proj_var = iters[3,], mean_err = iters[4,])

plot <- ggplot(data = plot_data,
               mapping = aes(x = V1, y = V2, col = proj_var)) +
  ggtitle("Projected Variance Dependence on Direction") +
  xlab("V1 Component") + ylab("V2 Component") +
  coord_cartesian(xlim = c(-1.1, 1.1),
                  ylim = c(-1.1, 1.1),
                  clip = "off") +
  scale_colour_gradient(low = "lightgrey", high = "red", name = "Variance of Projected Data") +
  geom_point() +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

plot <- plot + geom_segment(aes(x = -firstPC[1]*1e3, y = -firstPC[2]*1e3,
                                xend = firstPC[1]*1e3, yend = firstPC[2]*1e3),
                            colour = "green", lwd = 0.25)

plot2 <- ggplot(data = plot_data,
               mapping = aes(x = V1, y = V2, col = mean_err)) +
  ggtitle("Mean Approximation Error Dependence on Direction") +
  xlab("V1 Component") + ylab("V2 Component") +
  coord_cartesian(xlim = c(-1.1, 1.1),
                  ylim = c(-1.1, 1.1),
                  clip = "off") +
  scale_colour_gradient(low = "lightgrey", high = "red", name = "Mean Error") +
  geom_point() +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

plot2 <- plot2 + geom_segment(aes(x = -firstPC[1]*1e3, y = -firstPC[2]*1e3,
                                 xend = firstPC[1]*1e3, yend = firstPC[2]*1e3),
                             colour = "green", lwd = 0.25)

print(plot)
print(plot2)


# Gradient method ----

# Add Another Variable
V3 <- 10 + rnorm(N, V1 + V2, sd = 0.2)

# Format as a matrix
mat <- rbind(V1, V2, V3)

# Distance matrix(thing we want to approximate)
dist_mat <- dist(t(mat))


gradient_eval1 <- function(set_of_vecs, cur_vec, e = 1e-5) {
  dim <- length(cur_vec)

  gradient <- sapply(1:dim, function(i) {
    h_vec <- cur_vec
    h_vec[i] <- h_vec[i] + e

    fd <- ( orth_proj_var(set_of_vecs, h_vec) - orth_proj_var(set_of_vecs, cur_vec) ) / e

    return(fd)
  })

  return(gradient)
}

gradient_eval2 <- function(set_of_vecs, cur_vec, e = 1e-5) {
  dim <- length(cur_vec)

  gradient <- sapply(1:dim, function(i) {
    h_vec <- cur_vec
    h_vec[i] <- h_vec[i] + e

    fd <- ( var(orthogonal_projection(set_of_vecs, h_vec)) - var(orthogonal_projection(set_of_vecs, cur_vec)) ) / e

    return(fd)
  })

  return(gradient)
}

cur_vec <- rnorm(2)
cur_vec <- cur_vec / sum(cur_vec^2)^0.5

for (i in 1:1e3) {
  print(cur_vec)
  prop_vec <- cur_vec + gradient_eval(mat, cur_vec, 1e-6)
  prop_vec <- prop_vec / sum(prop_vec^2)^0.5

  if ( sum((prop_vec - cur_vec)^2)^0.5 < 1e-6 ) {
    break
  }

  cur_vec <- prop_vec

}

grad_firstPC <- cur_vec



# Generate data
Nobs <- 1e5
Nfeat <- 100
mat <- rbind(rnorm(Nobs, 0, sd = 10))

for (i in 2:Nfeat) {
  mat <- rbind(mat,
               rnorm(Nobs,
                     mean = colSums(mat * rnorm(nrow(mat), sd = 0.5)),
                     sd = rexp(Nobs, rate = 1))
               )
}

x <- my_pca(mat, 5, 1e-7, 1e-5, 100)

