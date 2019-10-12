svm <- function(TR, YTR)
{
  set.seed(123)
  cols <- ncol(TR)
  char_rows <- 12
  
  linear_model <- LiblineaR(data=TR, target=YTR, type=1, cost=0.01, bias=TRUE,
                            verbose=FALSE)
  return(linear_model)
}