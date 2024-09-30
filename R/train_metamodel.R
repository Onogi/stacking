train_metamodel <- function(basemodel_train_result, which_to_use, Metamodel, cross_validation = FALSE, TrainEachFold = FALSE, Training_X_list = NULL){

  #=>basemodel_train_result（train_basemodelの出力）の要素にTraining_X_listが含まれているので、引数にはいらないのでは？
  #=>またcross_validationを、basemodel_train_resultの要素とすると、引数に入れる必要はなくなります。
  
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
  
  if(cross_validation){
    if(TrainEachFold){
      ly <- length(basemodel_train_result$Y.randomised)
      nfold <- basemodel_train_result$Nfold
      if(ly %% nfold == 0){
        xsoeji <- matrix(1:ly, nrow = ly %/% nfold, ncol = nfold)
      } else {
        xsoeji <- matrix(0, nrow = ly %/% nfold + 1, ncol = nfold)
        xsoeji[1:ly] <- 1:ly
      }
      metamodel <- as.list(numeric(nfold))
      for(fold in 1:nfold){
        test <- xsoeji[, fold]
        metamodel[[fold]] <- train(valpr[test, ],
                                   basemodel_train_result$Y.randomised[test],
                                   method = Metamodel)
      }
    } else {
      metamodel <- train(valpr, basemodel_train_result$Y.randomised, method = Metamodel)
    }
    
  } else {
    
    #randomselect
    if(TrainEachFold){
      ly <- length(basemodel_train_result$Y.randomised)
      nfold <- basemodel_train_result$Nfold
      if(ly %% nfold == 0){
        xsoeji <- matrix(1:ly, nrow = ly %/% nfold, ncol = nfold)
      } else {
        xsoeji <- matrix(0, nrow = ly %/% nfold + 1, ncol = nfold)
        xsoeji[1:ly] <- 1:ly
      }
      #=>このxsoejiはcross_validation=TRUEのときに学習用サンプルを指定する機能をするものです。
      #=>ランダムサンプリングのときは、別に考える必要があります。
      
      metamodel <- as.list(numeric(nfold))
      for(fold in 1:nfold){
        test <- xsoeji[, fold]
        #Combine the test portion of valpr and the corresponding part of X.randomised vertically
        combined_data <- rbind(valpr[test, ], Training_X_list[[fold]])

        #=>xsoejiから作るtestも、cross_validation=TRUEのときに機能するものです
        
        metamodel[[fold]] <- train(combined_data,
                                   basemodel_train_result$Y.randomised[test],
                                   method = Metamodel)

        #=>cross_validation = FALSEのときは、Xが強制的にmetamodelのtrainingに使われているようです。
        #=>一方でcross_validation = TRUEのときは、Xはmetamodelのtrainingに使われていません。
        #=>cross validationであってもなくても、Xをmetamodelのtrainingに加えるか否か、引数で指定する必要があるでしょう。
      }
    } else {
      #Combine all of valpr and the saved X.randomised vertically
      combined_X_randomised <- do.call(rbind, Training_X_list)
      combined_data <- rbind(valpr, combined_X_randomised)
      metamodel <- train(combined_data, basemodel_train_result$Y.randomised, method = Metamodel)

      #=>ここも78から80行目と同じコメントです
    }
    
    metamodel_train_result <- list(train_result = metamodel,
                                   which_to_use = which_to_use,
                                   TrainEachFold = TrainEachFold)
    metamodel_train_result
    #=>metamodel_train_resultが、cross_validation=TRUEのときにも出力されるようにしないといけません
  }
  }

