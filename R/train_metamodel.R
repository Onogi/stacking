train_metamodel <- function(basemodel_train_result, which_to_use, Metamodel, TrainEachFold = FALSE, use_X = FALSE){
#元のXを含むことをuse_Xとしているがより適切な名称があれば変更する。
  
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
    ################################################################################################
    #basemodel_train_resultのYの要素名がcross-validationとrandom samplingで異なる場合は、ここでエラーになる
    ################################################################################################
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
          x_data <- cbind(valpr[test, ], basemodel_train_result$Training_X[test, ])
          metamodel[[fold]] <- train(x_data,
                                     basemodel_train_result$Y.randomised[test],
                                     method = Metamodel)
        }
      } else {
        x_data <- cbind(valpr, basemodel_train_result$Training_X)
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
    ############################################
    #ここもTrainEachFoldで2つに分けてもいいのでは？
    #TRUE：random samplingのiterationごとに学習
    #FALSE：まとめて学習
    ############################################
    # Training meta models (Random select)
    if (use_X) {
      X_combined <- do.call(rbind, basemodel_train_result$Training_X)
      feature_aggregation <- cbind(X_combined, basemodel_train_result$valpr)
      
      metamodel <- train(feature_aggregation, basemodel_train_result$Y_stacked, method = Metamodel)
    } else {
      metamodel <- train(basemodel_train_result$valpr, basemodel_train_result$Y_stacked, method = Metamodel)
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
