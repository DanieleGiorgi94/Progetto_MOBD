reduce_sampling_frequency <- function(dataset)
{
  n_samples <- (ncol(dataset) - 2) / 6
  n_channels <- 6
  n1 <- 16 
  n2 <- 0
  f_fraction <- 4
  
  # Prendo una "finestra temporale" ridotta che permetta di tenere in considerazione solo il segnale dalla 
  # sua P300 in poi (ossia tagliando la parte iniziale del segnale che non risulta significativa in termini di
  # valutazione di uno stimolo target), riducendo ancor di più la possibilità di valutazioni errate dovute a 
  # "picchi" P precedenti (in termini di tempo in ms) 
  
  new_dataset <- matrix(0, nrow = nrow(dataset),
                ncol = n_samples * n_channels / f_fraction -
                       n1 * n_channels - n2 * n_channels)
  for (i in 1:nrow(dataset)) {
    for (k in 1:n_channels) {
      for (j in 1:(n_samples/f_fraction - n1 - n2)) {
        new_dataset[i, j + (k - 1) * (n_samples / f_fraction - n1 - n2)] <-
          dataset[i, n1 + j * f_fraction + (k - 1) * n_samples]
      }
    }
  }
  new_dataset <- cbind(new_dataset,
                       dataset[,(ncol(dataset) - 1):(ncol(dataset))])
  return(new_dataset)
}