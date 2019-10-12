delete_channels <- function(dataset, del1, del2)
{
  nchannels <- 8
  nsamples <- 204
  j <- 1
  
  red_data <- matrix(0, nrow = 3600, ncol = ncol(dataset)-nsamples*2)
  for (i in 1: (ncol(dataset))){
    if (i <= (del1-1)*nsamples |
        (i >= (del1*nsamples+1) & i <= (del2-1)*nsamples) |
        (i >= (del2*nsamples+1)) ){
      red_data[, j] <- dataset[,i] 
      j <- j+1
    }
  
  }
  return (red_data)
}