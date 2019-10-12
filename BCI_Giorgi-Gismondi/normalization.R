normalization <- function(my_data, scale_values) {
  data_test <- my_data[,1:(ncol(my_data)-2)]
  c_test <- my_data[,ncol(my_data)-1]
  label_test <- my_data[,ncol(my_data)]
  
  scaled_test <- scale(data_test, scale_values[1,], scale_values[2,])
  
  output <- list(scaled_test, c_test, label_test)
  names(output) <- c("scaled_test",
                     "c_test",
                     "label_test")
  return(output)
}