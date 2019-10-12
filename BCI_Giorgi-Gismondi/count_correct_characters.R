count_correct_characters <- function(classifications, true_y)
{
  rows <- nrow(y_test)
  char_rows <- 120
  
  no_incorrect_characters <- 0
  for (i in 1:(rows/120)) {
    if (sum(which((classifications[((i-1) * char_rows + 1):(i * char_rows)] ==
          y_test[((i-1) * char_rows + 1):(i * char_rows),]) == FALSE)) > 0)
      no_incorrect_characters <- no_incorrect_characters + 1
  }
  return(rows / char_rows - no_incorrect_characters)
}