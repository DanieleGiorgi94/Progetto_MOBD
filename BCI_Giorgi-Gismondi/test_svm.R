test_svm <- function(TR, YTR, TS, YTS)
{
  set.seed(123)
  cols <- ncol(TR)
  char_rows <- 12

  linear_model <- LiblineaR(data=TR, target=YTR, type=1, cost=0.01, bias=TRUE,
                            verbose=FALSE)
  
  #vettore delle classificazioni "finale"
  classifications <- vector(mode = "integer", length = length(YTS))
  
  for (k in 1:(nrow(TS)/char_rows)) {
    #vettore temporaneo delle classificazioni per ogni carattere
    y_vec <- c()
    
    #per ciascun carattere, calcolo w' * x + b, dove x sono le misurazioni
    #relative ad una determinata illuminazione di riga/colonna
    for (i in 1:char_rows) {
      y_vec <- cbind(y_vec,
            crossprod(linear_model$W[1:cols],
            TS[(k - 1) * char_rows + i,]) +
            linear_model$W[cols + 1])
    }
    
    #prendo il massimo w' * x + b, con x relativo all'illuminazione delle sole
    #righe e lo classifico come 1; gli altri sono assegnati a -1. (sono le
    #prime 6 componenti poiché il dataset era stato ordinato)
    max_i <- which.max(y_vec[1:6])
    y_vec[max_i] <- 1
    y_vec[1:6][-max_i] <- c(-1, -1, -1, -1, -1)
    
    #prendo il massimo w' * x + b, con x relativo all'illuminazione delle sole
    #colonne e lo classifico come 1; gli altri sono assegnati a -1. (sono le
    #ultime 6 componenti poiché il dataset era stato ordinato)
    max_i <- which.max(y_vec[7:12])
    y_vec[max_i + 6] <- 1
    y_vec[7:12][-max_i] <- c(-1, -1, -1, -1, -1)
    
    #"inserisco" il vettore temporaneo delle classificazioni nella posizione
    #opportuna del vettore delle classificazioni "finale"
    classifications[((k - 1) * char_rows + 1):(k * char_rows)] <-
      y_vec
  }
  
  #sommo il vettore delle Y del test set e le classificazioni effettuate dalla
  #SVM: tutte le componenti pari a 2 sono gli stimoli target ben classificati,
  #mentre quelle pari a -2 sono gli stimoli non target ben classificati.
  tmp <- YTS + classifications
  no_correct_target_points <- length(which(tmp == 2))
  no_correct_non_target_points <- length(which(tmp == -2))
  
  #la percentuale di target (non target) ben classificati è calcolata
  #considerando che per ogni 12 righe del dataset, necessariamente 2 sono
  #target e le altre 10 sono non target.
  res <- list(classifications,
              no_correct_target_points / (nrow(TS) / 12 * 2),
              no_correct_non_target_points / (nrow(TS) / 12 * 10))
  names(res) <- c("classification_vec",
                  "correct_target_points",
                  "correct_non_target_points")
  return(res)
}