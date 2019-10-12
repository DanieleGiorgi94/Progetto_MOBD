calculate_mean_matrix <- function(dataset)
{
  iter <- 10
  char_iter <- 12
  chars <- length(dataset)
  mean_matrix <- matrix(0,
                        nrow = nrow(dataset[[1]]) * chars / iter,
                        ncol = ncol(dataset[[1]]))
  cols <- ncol(dataset[[1]]) - 2
  
  new_dataset <- dataset
  for (k in 1:chars) {
    new_dataset[[k]] <-
      dataset[[k]][order(dataset[[k]][, ncol(dataset[[k]]) - 1]), ]
    
    for (i in 1:char_iter) {
      for (j in 1:cols) {
        mean_matrix[(k - 1) * char_iter + i, j] <- mean(
          new_dataset[[k]][((i - 1) * iter + 1):(i * iter), j])
      }
      mean_matrix[(k - 1) * char_iter + i, cols + 1] <-
        new_dataset[[k]][(i - 1) * iter + 1, cols + 1]
      mean_matrix[(k - 1) * char_iter + i, cols + 2] <-
        new_dataset[[k]][(i - 1) * iter + 1, cols + 2]
    }
  }
  
  return(mean_matrix)
}