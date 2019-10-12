svm_classifications <- function(w, b, TS, YTS)
{
  set.seed(123)
  char_rows <- 12
  
  classifications <- vector(mode = "integer", length = length(YTS))
  y_vec <- vector(mode = "integer", length = char_rows)
  for (k in 1:(nrow(TS)/char_rows)) {
    y_vec <- c()
    for (i in 1:char_rows) {
      y_vec <- cbind(y_vec,
        crossprod(w, TS[(k - 1) * char_rows + i,]) + b)
    }
    
    max_i <- which.max(y_vec[1:6])
    y_vec[max_i] <- 1
    y_vec[1:6][-max_i] <- c(-1, -1, -1, -1, -1)
    
    max_i <- which.max(y_vec[7:12])
    y_vec[max_i + 6] <- 1
    y_vec[7:12][-max_i] <- c(-1, -1, -1, -1, -1)
    
    classifications[((k - 1) * char_rows + 1):(k * char_rows)] <-
      y_vec
  }
  
  tmp <- YTS + classifications
  no_correct_target_points <- length(which(tmp == 2))
  no_correct_non_target_points <- length(which(tmp == -2))
  
  res <- list(classifications,
              no_correct_target_points / (nrow(TS) / 12 * 2),
              no_correct_non_target_points / (nrow(TS) / 12 * 10))
  names(res) <- c("classification_vec",
                  "correct_target_points",
                  "correct_non_target_points")
  return(res)
}