data_understanding <- function(my_data)
{
  set.seed(123)
  
  n_rows <- nrow(my_data)
  n_cols <- ncol(my_data)
  n_samples <- 204
  n_channels <- 8
  t <- seq(1, n_samples, 1) # Vettore dei tempi per il plot
  
  # Calcolo dei segnali di riferimento relativi ai canali 
  Y <- my_data[,length(my_data)]
  target_pos <- which(Y == 1)
  nontarget_pos <- which(Y == -1)
  
  # Riferimento target = media sui segnali con etichetta +1
  target_ref <- apply(my_data[target_pos,],2,mean)
  # Riferimento non target = media sui segnali con etichetta -1
  nontarget_ref <- apply(my_data[nontarget_pos,],2,mean)
  
  for (ch in 1:n_channels){
    # plot relativo al singolo canale
    plot(t, target_ref[((ch-1)*n_samples+1):(ch*n_samples)],
         main="Segnali di riferimento", 
         type="l",col="blue",
         xlab="time", 
         ylab="potential (uV)")
    lines(t,nontarget_ref[((ch-1)*n_samples+1):(ch*n_samples)],
          type="l",
          col="red")
    legend("topleft",
           c("Target","Non Target"),
           fill=c("blue","red"))
  }
  
  # restituisce i segnali di riferimento
  output <- rbind(target_ref, nontarget_ref)
  return(output)
}
