add_energy <- function(dataset)
{
  cols <- (ncol(dataset) - 3) / 2
  
  energy_col <- matrix(0, nrow = nrow(dataset), ncol = 1)
  for (i in 1:nrow(dataset)) {
    energy_col[i] <- crossprod(dataset[i, 1:cols],
                               dataset[i, 1:cols])
  }
  
  # Ricordiamo che l'energia del segnale (a tempo discreto) è calcolata attraverso la somma del prodotto scalare del segnale
  # per se stesso (segnale al quadrato)
  
  return(cbind(
    dataset[, 1:(ncol(dataset)-3)],
    energy_col,
    dataset[, (ncol(dataset)-2):ncol(dataset)]
  ))
}