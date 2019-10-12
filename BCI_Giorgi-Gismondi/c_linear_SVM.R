c_linear_SVM <- function(TR, YTR, TS, YTS, c)
{
  set.seed(123)
  cols <- ncol(TR)
  char_rows <- 12
  
  linear_model <- LiblineaR(data=TR, target=YTR, type=1, cost=c, bias=TRUE,
                            verbose=FALSE)
  
  classifications <- vector(mode = "integer", length = length(YTS))
  y_vec <- vector(mode = "integer", length = char_rows)
  for (k in 1:(nrow(TS)/char_rows)) {
    y_vec <- c()
    for (i in 1:char_rows) {
      y_vec <- cbind(y_vec,
                     crossprod(linear_model$W[1:cols],
                               TS[(k - 1) * char_rows + i,]) +
                       linear_model$W[cols + 1])
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
  
  res <- list(no_correct_target_points / (nrow(TS) / 12 * 2))
  names(res) <- c("correct_target_points")
  return(res)
}