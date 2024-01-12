library(data.tree)
library(parallel)

# Functions
divide <- function(parent) {
  parent$AddChild("D1", time_to_division = rgamma(1, 5, 1/5))
  parent$AddChild("D2", time_to_division = rgamma(1, 5, 1/5))


  parent$D1$time_elapsed <-
    parent$D2$time_elapsed <-
    sum(parent$D1$Get("time_to_division", traversal = "ancestor")[-1])

  NULL
}

iterate <- function(tree) {
  lapply(tree$leaves, function(leaf) {
    divide(leaf)
  })

  NULL
}



# Generate Data
cl <- parallel::makeCluster(7)

parallel::clusterEvalQ(cl, {
  library(data.tree)

  # Functions
  divide <- function(parent) {
    parent$AddChild("D1", time_to_division = rgamma(1, 5, 1/5))
    parent$AddChild("D2", time_to_division = rgamma(1, 5, 1/5))


    parent$D1$time_elapsed <-
      parent$D2$time_elapsed <-
      sum(parent$D1$Get("time_to_division", traversal = "ancestor")[-1])

    NULL
  }

  iterate <- function(tree) {
    lapply(tree$leaves, function(leaf) {
      divide(leaf)
    })

    NULL
  }


  time_point <- 100

  NULL
})


dist <- parallel::clusterEvalQ(cl, {
  idist <- replicate(1500, {
    history <- Node$new("Steve", time_to_division = rgamma(1, 5, 1/5), time_elapsed = 0)

    replicate(8, iterate(history))

    Prune(history, function(node) { node$time_elapsed < time_point })
    history$leafCount # Number of cells alive at time T
  })
}) |> unlist()

parallel::stopCluster(cl)
