low_pass_filter <- function(dataset)
{
  n_channels <- 6 #numero di canali ridotto a seguito dell'eliminazione dei due canali "rumorosi"
  n_samples <- 204 #numero campioni dei segnali 
  
  new_dataset <- dataset
  
  for (k in 1:n_channels) {
    for (i in 1:nrow(dataset)) {
      for (j in ((k - 1) * n_samples + 2):(k * n_samples)) {
        new_dataset[i, j] <- new_dataset[i, j-1] +
          25/128 * (dataset[i, j] - new_dataset[i, j-1])
      }
    }
  }
  
  # Low_pass_filter ->   y[i] := y[i-1] + α * (x[i] - y[i-1]), dove nel nostro caso α = 25/128 è la costante 
  # che permette un decremento frequenziale a 25 Hz, mentre y := segnale filtrato ed x := segnale originale 
  
  return(new_dataset)
}