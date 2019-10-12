get_scale_values <- function(my_data)
{
    data_train <- my_data[,1:(ncol(my_data)-2)]
    c_train <- my_data[,ncol(my_data)-1]
    label_train <- my_data[,ncol(my_data)]
    
    scaled_training <- scale(data_train, center = T, scale = T)
    
    return(rbind(attr(scaled_training, "scaled:center"),
             attr(scaled_training, "scaled:scale")))
}