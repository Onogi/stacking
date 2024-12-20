train_metamodel <- function(X, basemodel_train_result, which_to_use, Metamodel, use_X = FALSE, TrainEachFold = FALSE){
  
  nb <- basemodel_train_result$no_base
  
  if(is.null(which_to_use)) which_to_use <- 1:nb
  
  # Checking which_to_use
  if(length(which_to_use) > nb)
    stop("Length of which_to_use must not exceed ", nb)
  
  if(0 < sum(which_to_use > nb))
    stop("which_to_use contains larger values than the number of base models (", nb,")")
  
  valpr <- basemodel_train_result$valpr[, which_to_use, drop = FALSE]
  
  if(basemodel_train_result$Type == "Classification"){
    Category <- sort(unique(basemodel_train_result$Y.randomised))
    # Add all categories to each base model output (to make model.matrix outputs same as prediction)
    Addline <- matrix(Category, nrow = length(Category), ncol = nb)
    valpr <- rbind(valpr, Addline)
    valpr <- data.frame(valpr)
    valpr <- model.matrix(~., data = valpr)
    # Remove added lines
    valpr <- valpr[-c((nrow(valpr) - length(Category) + 1):nrow(valpr)), ]
  }
  
  if (basemodel_train_result$cross_validation) {
    
    # Training meta models
    if (use_X) {  
      X.narm <- X[basemodel_train_result$which_valid, ]
      Training_X　<- X.narm[basemodel_train_result$Order, ]
      
      if (TrainEachFold) {
        ly <- length(basemodel_train_result$Y.randomised)
        nfold <- basemodel_train_result$Nfold
        if (ly %% nfold == 0) {
          xsoeji <- matrix(1:ly, nrow = ly %/% nfold, ncol = nfold)
        } else {
          xsoeji <- matrix(0, nrow = ly %/% nfold + 1, ncol = nfold)
          xsoeji[1:ly] <- 1:ly
        }
        metamodel <- as.list(numeric(nfold))
        for (fold in 1:nfold) {
          test <- xsoeji[, fold]
          x_data <- cbind(valpr[test, ], Training_X[test, ])
          if(basemodel_train_result$Type == "Classification"){
            colnames(x_data)[(ncol(valpr) + 1):ncol(x_data)] <- as.character(1:(ncol(x_data) - ncol(valpr)))
          } else {
            colnames(x_data) <- as.character(1:ncol(x_data))
          }
          
          metamodel[[fold]] <- train(x_data,
                                     basemodel_train_result$Y.randomised[test],
                                     method = Metamodel)
        }
      } else {
        x_data <- cbind(valpr, Training_X)
        if(basemodel_train_result$Type == "Classification"){
          colnames(x_data)[(ncol(valpr) + 1):ncol(x_data)] <- as.character(1:(ncol(x_data) - ncol(valpr)))
        } else {
          colnames(x_data) <- as.character(1:ncol(x_data))
        }
        metamodel <- train(x_data, basemodel_train_result$Y.randomised, method = Metamodel)
      }
    } else {
      if (TrainEachFold) {
        ly <- length(basemodel_train_result$Y.randomised)
        nfold <- basemodel_train_result$Nfold
        if (ly %% nfold == 0) {
          xsoeji <- matrix(1:ly, nrow = ly %/% nfold, ncol = nfold)
        } else {
          xsoeji <- matrix(0, nrow = ly %/% nfold + 1, ncol = nfold)
          xsoeji[1:ly] <- 1:ly
        }
        metamodel <- as.list(numeric(nfold))
        for (fold in 1:nfold) {
          test <- xsoeji[, fold]
          metamodel[[fold]] <- train(valpr[test, ],
                                     basemodel_train_result$Y.randomised[test],
                                     method = Metamodel)
        }
      } else {
        metamodel <- train(valpr, basemodel_train_result$Y.randomised, method = Metamodel)
      }
    }
    
    #Output training results
    metamodel_train_result <- list(train_result = metamodel,
                                   which_to_use = which_to_use,
                                   cross_validation = basemodel_train_result$cross_validation,
                                   use_X = use_X,
                                   TrainEachFold = TrainEachFold)
    
  } else {
    # Training meta models (Random select)
    num_sample <- basemodel_train_result$num_sample
    chunk_size <- nrow(basemodel_train_result$valpr)/num_sample
    
    if (use_X) {
      X.narm <- X[basemodel_train_result$which_valid,　]
      
      if (TrainEachFold) {
        metamodel <- as.list(numeric(num_sample))
        for (iteration in 1:num_sample) {
          start_row <- (iteration - 1) * chunk_size + 1
          end_row <- start_row + chunk_size - 1
          valpr_piece <- valpr[start_row:end_row, ]
          Y.randomised <- basemodel_train_result$Y.randomised[start_row:end_row, ]
          X.randomised <-  X.narm[-basemodel_train_result$Order[[iteration]],　]
          feature_aggregation <- cbind(valpr_piece, X.randomised)
          if(basemodel_train_result$Type == "Classification"){
            colnames(feature_aggregation)[(ncol(valpr_piece) + 1):ncol(feature_aggregation)] <- as.character(1:(ncol(feature_aggregation) - ncol(valpr_piece)))
          } else {
            colnames(feature_aggregation) <- as.character(1:ncol(feature_aggregation))
          }
          metamodel[[iteration]] <- train(feature_aggregation, Y.randomised, method = Metamodel)
        }
      }else{
        Training_X_list <- list()
        for (iteration in 1:num_sample) {
          X.randomised <- X.narm[-basemodel_train_result$Order[[iteration]],　]
          Training_X_list[[iteration]] <- X.randomised
        }
        Training_X <- do.call(rbind, Training_X_list)
        feature_aggregation <- cbind(valpr, Training_X)
        if(basemodel_train_result$Type == "Classification"){
          colnames(feature_aggregation)[(ncol(valpr) + 1):ncol(feature_aggregation)] <- as.character(1:(ncol(feature_aggregation) - ncol(valpr)))
        } else {
          colnames(feature_aggregation) <- as.character(1:ncol(feature_aggregation))
        }
        Y.randomised <- as.vector(basemodel_train_result$Y.randomised)
        metamodel <- train(feature_aggregation, Y.randomised, method = Metamodel)
      }
    } else {
      if (TrainEachFold) {
        metamodel <- as.list(numeric(num_sample))
        for (iteration in 1:num_sample) {
          start_row <- (iteration - 1) * chunk_size + 1
          end_row <- start_row + chunk_size - 1
          valpr_piece <- valpr[start_row:end_row, ]
          Y.randomised <- basemodel_train_result$Y.randomised[start_row:end_row, ]
          metamodel[[iteration]] <- train(valpr_piece, Y.randomised, method = Metamodel)
        }
      }else{
        Y.randomised <- as.vector(basemodel_train_result$Y.randomised)
        metamodel <- train(valpr, Y.randomised, method = Metamodel)
      }
    }
    
    #Output training results
    metamodel_train_result <- list(train_result = metamodel,
                                   which_to_use = which_to_use,
                                   cross_validation = basemodel_train_result$cross_validation,
                                   use_X = use_X,
                                   TrainEachFold = TrainEachFold)
  }
  
  return(metamodel_train_result)  
}
