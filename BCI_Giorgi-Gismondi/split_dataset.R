split_dataset <- function(data, word)
{
  c <- 60
  indexes <- ((word - 1) * c + 1):(word * c)
  training <- data[-indexes,]
  test <- data[indexes,]
  
  data_split <- list(training, test)
  names(data_split) <- c("training_set", "test_set")
  return(data_split)
}