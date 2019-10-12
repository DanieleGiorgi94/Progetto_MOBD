adapt_classifications <- function(ord_y, c_vec)
{
  rows <- 120
  n <- length(ord_y) / rows

  vec <- matrix(
    0,
    nrow = length(ord_y),
    ncol = 1
  )
  
  for (i in 1:n) {
    tmp <- which(ord_y[((i - 1) * rows + 1):(i * rows),] == 1)
    selected_row <- tmp[1]
    selected_col <- tmp[2]
    tmp_indexes <-
      which(c_vec[((i - 1) * rows + 1):(i * rows),] == selected_row) + 
      (i - 1) * rows
    vec[tmp_indexes] <- matrix(1, nrow = 10, ncol = 1)
    tmp_indexes <-
      which(c_vec[((i - 1) * rows + 1):(i * rows),] == selected_col) +
      (i - 1) * rows
    vec[tmp_indexes] <- matrix(1, nrow = 10, ncol = 1)
  }
  #ci sono 10 '-1' ogni 12 righe moltiplicato per 10 iterazioni, moltiplicato
  #per il numero di caratteri: questo è il numero complessivo di '-1'
  tmp <- 10 * 10 * nrow(c_test) / 120
  vec[which(vec == 0),] <- matrix(-1, nrow = tmp, ncol = 1)
  return(vec)
}