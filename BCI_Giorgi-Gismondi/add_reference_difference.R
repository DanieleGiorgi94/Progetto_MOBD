add_reference_difference <- function(dataset, references)
{
  n_channels <- 6
  n_samples <- ncol(references)
  differences <- matrix(0, nrow = nrow(dataset), ncol = n_channels * n_samples)
  variances <- matrix(0, nrow = nrow(dataset), ncol = 1)
  
  for (k in 1:n_channels) {
    for (i in 1:nrow(dataset)) {
      for (j in 1:n_samples) {
        differences[i, (k - 1) * n_samples + j] <- abs(
          dataset[i, (k - 1) * n_samples + j] - references[k, j])
      }
      variances[i] <- var(differences[i, ])
    }
  }
  
  new_dataset <- cbind(dataset[,1:(ncol(dataset)-2)],
                       differences,
                       variances,
                       dataset[,(ncol(dataset)-1):ncol(dataset)])
  return(new_dataset)
}