extend_classifications <- function(y_vec)
{
  iterations <- 10
  char_iterations <- 12
  n <- length(y_vec) / 12 #numero di caratteri classificati
  lines <- char_iterations * iterations
  
  vec <- matrix(
    0,
    nrow = length(y_vec) * 10,
    ncol = 1
  )
  
  for (i in 1:n) {
    for (j in 1:iterations) {
      vec[((i - 1) * lines + 1):(i * lines)] <-
        y_vec[((i - 1) * char_iterations + 1):(i * char_iterations)]
    }
  }
  return(vec)
}