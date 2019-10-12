split_by_char <- function(dataset)
{
  no_subdatasets <- nrow(dataset) / 120
  subdatasets <- vector(mode = "list", length = no_subdatasets)
  subdatasets <- lapply(subdatasets,
      function(X) matrix(nrow = 120, ncol = ncol(dataset) - 2))
  
  for (k in 1:no_subdatasets)
        subdatasets[[k]] <- dataset[((k-1)*120+1):(120*k),]
  
  return(subdatasets)
}