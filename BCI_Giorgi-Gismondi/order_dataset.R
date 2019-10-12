order_dataset <- function(dataset)
{
  char_rows <- 12
  n_cols <- ncol(dataset) - 1
  
  no_subdatasets <- nrow(dataset) / 12
  subdatasets <- vector(mode = "list", length = no_subdatasets)
  subdatasets <- lapply(subdatasets,
            function(X) matrix(nrow = 12, ncol = ncol(dataset) - 2))
  
  for (k in 1:no_subdatasets) {
    subdatasets[[k]] <- dataset[((k-1)*12+1):(12*k),]
  }
  
  ordered_dataset <- matrix(0, nrow = nrow(dataset), ncol = ncol(dataset))
  for (k in 1:no_subdatasets) {
    tmp <- subdatasets[[k]]
    tmp <- tmp[order(tmp[, n_cols], decreasing = FALSE),]
    for (j in 1:12) {
      for (i in 1:ncol(tmp)) {
        ordered_dataset[(k-1)*12+j, i] <- as.numeric(tmp[j, i])
      }
    }
  }
  
  return(ordered_dataset)
}