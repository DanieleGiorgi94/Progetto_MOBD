source("c_linear_SVM.R")
setting_linear_SVM <- function(dataset_list){
  set.seed(123)
  c_vector <- c(0.001, 0.005, 0.01, 0.05, 0.1)
  n_cost <- 1:5
  n_folds <- 5
  c <- 60
  
  folds_i <- sample(rep(0:(n_folds - 1),
          length.out = nrow(dataset_list$scaled_training) / 12))
  
  classification_parameter <- lapply(n_cost, function(n_classifiers)
    matrix(0, nrow = n_folds, ncol = 1))
  names(classification_parameter) <- c("c0.001",
                                       "c0.005",
                                       "c0.01",
                                       "c0.05",
                                       "c0.1")
  
  for(j in 1:length(c_vector)) {
    for (k in 1:n_folds) {
      
      train_indexes <- (folds_i[k] * c + 1):((folds_i[k] + 1) * c)
      
      train <- dataset_list$scaled_training[-train_indexes, ]
      test <- dataset_list$scaled_training[train_indexes, ]
      
      label_train <- dataset_list$label_train[-train_indexes]
      label_test <- dataset_list$label_train[train_indexes]
      
      classification_parameter[[j]][k] <- c_linear_SVM(train,
              label_train, test, label_test, c_vector[j])
    }
  }
  return(classification_parameter)
}