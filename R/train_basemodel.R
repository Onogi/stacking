train_basemodel <- function(X, Y, Nfold, Method, core = 1, cross_validation = FALSE, num_sample = 10, proportion = 0.8){

  ##############################################################################################################################################
  #num_sampleはランダムサンプリングのときしか使われないので、引数の後ろの方に回した方がよいです。また適当な初期値を与えておきましょう（num_sample = 10あたり）
  ##############################################################################################################################################
  if(is.factor(Y)) {
    Type <- "Classification"
    Y <- as.character(Y)
  } else {
    Type <- "Regression"
    Y <- as.numeric(Y)
    if(sum(is.na(Y)) == length(Y))
      stop("All elements in Y may be NAs. They may be characters.
           Use factor when classification")
  }
  
  #Removing NA from Y
  colnames(X) <- 1:ncol(X)
  Y.narm <- Y[!is.na(Y)]
  X.narm <- X[!is.na(Y), ]
  lY <- length(Y.narm)
  if(length(lY) == 0) stop("Elements of Y are all NA.")
  
  #Checking the names of methods
  lM <- length(Method)
  check <- numeric(length = lM)
  AvailableModel <- names(getModelInfo())
  for(m in 1:lM){
    if(!is.element(names(Method)[m], AvailableModel)){
      warning(names(Method)[[m]], " is not available in caret")
      check[m] <- 1
    }
  }
  if(any(check>0)){stop("Please confirm the names/spellings of the following methods")}
  
  #Checking the names and numbers of hyperparameters for each method
  check <- numeric(length = lM)
  for(m in 1:lM){
    if(!setequal(colnames(Method[[m]]), modelLookup(names(Method)[m])$parameter)){
      warning("Hyperparameters of ", names(Method)[[m]], " is incorrect")
      check[m] <- 1
    }
  }
  if(any(check>0)){stop("Please confirm the numbers and/or names of hyperparameters of the following methods")}
  
  method <- names(Method)
  
  #Determine hyperparameter values when values are not specified
  for(m in 1:lM){
    if(any(colSums(is.na(Method[[m]])) == nrow(Method[[m]]))){
      result_temp <- train(X.narm, Y.narm, method = method[m])
      for(j in colnames(Method[[m]])){
        if(sum(is.na(Method[[m]][, j])) == nrow(Method[[m]]))
          Method[[m]][, j] <- c(result_temp$bestTune[, j], rep(NA, nrow(Method[[m]]) - 1))
      }
    }
  }
  
  #Generating the combinations of hyperparameters
  hyp2give <- as.list(numeric(lM))
  for(i in 1:lM){
    hyp2give[[i]]<-expand.grid(Method[[i]])
    if(any(is.na(hyp2give[[i]]))){
      NotUse <- rowSums(is.na(hyp2give[[i]]))
      hyp2give[[i]] <- hyp2give[[i]][NotUse == 0, ]
    }
  }
  
  #List required for parallel computing
  L <- NULL
  for(i in 1:length(hyp2give)){
    for(j in 1:nrow(hyp2give[[i]])){
      L <- c(L, list(list(hyp = hyp2give[[i]][j, , drop = FALSE],
                          method = method[i])))
    }
  }
  
  #When core > length(L)
  if(length(L) > core){
    if(length(L) %% core == 0){
      Repeat.parLapply <- length(L) / core
      Division <- matrix(1:length(L), ncol = Repeat.parLapply)
    }else{
      Repeat.parLapply <- length(L)%/% core + 1
      Division <- matrix(0, nrow = core, ncol = Repeat.parLapply)
      Division[1:length(L)] <- 1:length(L)
    }
  }else{
    Repeat.parLapply <- 1
    Division <- matrix(1:length(L), ncol = Repeat.parLapply)
  }
  
  basemodel_train_result <- list()
  
  if (cross_validation) {
    
    # Dividing data for cross-validation
    ORDER <- sample(1:lY, lY, replace = FALSE)
    Y.randomised <- Y.narm[ORDER]
    X.randomised <- X.narm[ORDER, ]
    
    if (lY %% Nfold == 0) {
      xsoeji <- matrix(1:lY, nrow = lY %/% Nfold, ncol = Nfold)
    } else {
      xsoeji <- matrix(0, nrow = lY %/% Nfold + 1, ncol = Nfold)
      xsoeji[1:lY] <- 1:lY
    }
    
    # Training base models
    train_result <- as.list(numeric(Nfold))
    
    valpr <- matrix(nrow = lY, ncol = length(L))
    colnames(valpr) <- 1:length(L)
    
    for (fold in 1:Nfold) {
      cat("CV fold", fold, "\n")
      Test <- xsoeji[, fold]
      train_result[[fold]] <- train_basemodel_core(Repeat.parLapply,
                                                   Division,
                                                   L,
                                                   core,
                                                   X.randomised,
                                                   Y.randomised,
                                                   Test)
      
      # Creating explanatory variables for the meta model
      x.test <- X.randomised[Test, ]
      if (Type == "Classification") {
        for (k in 1:length(L)) {
          valpr[Test, k] <- as.character(predict(train_result[[fold]][[k]], x.test))
        }
      } else {
        for (k in 1:length(L)) {
          valpr[Test, k] <- predict(train_result[[fold]][[k]], x.test)
        }
      }
    }
    
    # Output training results
    basemodel_train_result <- list(
      train_result = train_result,
      no_base = length(L),
      valpr = valpr,
      Y.randomised = Y.randomised,
      Order = ORDER,
      Type = Type,
      Nfold = Nfold,
      Training_X = X.randomised,
      cross_validation = cross_validation
    )
    
  } else {  
    
    # Training base models (Random select)
    if (is.null(proportion) || proportion <= 0 || proportion > 1) {
      stop("proportion must be a real number greater than 0 and less than or equal to 1.")
    }
    
    train_result <- as.list(numeric(num_sample))
    Training_X <- as.list(numeric(num_sample))
    ORDER <- as.list(numeric(num_sample))

    sample_size <- round(proportion * lY)
    sample_rows <- round((lY - sample_size) * num_sample)
    if (sample_rows < 1) {
    stop("Error: The number of sample rows is less than 1. Adjust the proportion value.")
}
    valpr <- matrix(nrow = sample_rows, ncol = length(L))
    Y_stacked <- matrix(nrow = sample_rows, ncol = 1)
    ####################################################################################################################
    #proportion * lYが整数でないことを考える必要があります。roundで丸めるとよいでしょう。
    #またproportion * lYが1未満のときはエラーで止まった方が無難そうです。
    #####################################################################################################################
    colnames(valpr) <- 1:length(L)
    
    for (iteration in 1:num_sample) {
      cat("CV Random sampling iteration", iteration, "\n")
      ##############################################################
      #このメッセージは"Random sampling iteration"にしておきましょうか
      ##############################################################
      
      # Randomly select training instances
      ORDER[iteration] <- sample(1:lY, size = sample_size, replace = FALSE)
      ################################################################################################################
      #ここはroundで丸めてますね。最初に一回丸めて、その数字を適当なオブジェクトに格納し、あとでそれを使いまわすとよいと思います。
      #################################################################################################################
      
      # Use the rest of the instances as test set
      Test <- setdiff(1:lY, ORDER[iteration]) 
      Y.randomised <- Y.narm[ORDER[iteration]]
      X.randomised <- X.narm[ORDER[iteration], ]
      
      # Save X.randomised in a list
      Training_X[[iteration]] <- X.randomised
      
      # Train the base models
      train_result[[iteration]] <- train_basemodel_core(Repeat.parLapply,
                                                        Division,
                                                        L,
                                                        core,
                                                        X.randomised,
                                                        Y.randomised,
                                                        Test)
      
      # Creating explanatory variables for the meta model
      x.test <- X.narm[Test, ]
      start_row <- (iteration - 1) * (lY - sample_size) + 1
      end_row <- iteration * (lY - sample_size)
      
      if (Type == "Classification") {
      for (j in 1:length(L)) {
      valpr[start_row:end_row, j] <- as.character(predict(train_result[[iteration]][[j]], x.test))
      }
      } else {
      for (j in 1:length(L)) {
      valpr[start_row:end_row, j] <- predict(train_result[[iteration]][[j]], x.test)
      }
    }
      ################################################################################################################################################
      #train_basemodel_coreの戻り値は、base modelの数の長さのリストです。そのため、train_result[[iteration]]は長さlength(L)のリストになります。
      #Testデータに対する予測値をvalprに格納するところでは、train_result[[iteration]]の各要素（base model）にx.testを与えて予測値を得、それをvalprに格納していきます。
      #cross-validationのときの129～135行目をみると、for文でbase modelの予測値を1つずつ得ているのが分かると思います。
      #################################################################################################################################################
      Y_stacked[start_row:end_row, ] <- Y.randomised
    }
    
    # Output training results
    basemodel_train_result <- list(
      train_result = train_result,
      no_base = length(L),
      valpr = valpr,
      Y.randomized = Y_stacked,
      Order = ORDER,
      Type = Type,
      num_sample = num_sample,
      proportion = proportion,
      Training_X = Training_X,
      cross_validation = cross_validation
    )
    #####################################################################################################
    #リストの要素名は、cross-validationもrandom samplingも同じがよいと思います。Y.randomizedでいいと思います。
    ########################################################################################################
  }
  
  return(basemodel_train_result)
}
