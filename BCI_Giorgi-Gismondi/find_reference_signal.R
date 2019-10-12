find_reference_signal <- function(dataset)
{
  cols <- ncol(dataset)
  n_channels <- 6
  rows <- nrow(dataset)
  no_ref_samples <- (cols - 2) / n_channels
  
  reference_samples <- dataset[which(dataset[, cols] == 1),]
  
  reference_samples_mean <- matrix(0, nrow = n_channels, ncol = no_ref_samples)
  for (k in 1:no_ref_samples) {
    for (i in 1:nrow(reference_samples)) {
      for (j in 1:n_channels) {
        reference_samples_mean[j, k] <- reference_samples_mean[j, k] +
          reference_samples[i, (j-1) * no_ref_samples + k] /
          (no_ref_samples)
      }
    }
  }
  return(reference_samples_mean)
}