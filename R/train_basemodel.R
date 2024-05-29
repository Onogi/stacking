train_basemodel <- function(X, Y, Nfold, Method, core = 1, cross_validation = TRUE, number = NULL){
  
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
  
  if(cross_validation){
    
    #Dividing data for cross-validation
    ORDER <- sample(1:lY, lY, replace=FALSE)
    Y.randomised <- Y.narm[ORDER]
    X.randomised <- X.narm[ORDER, ]
    
    if(lY%%Nfold == 0){
      xsoeji <- matrix(1:lY, nrow = lY %/% Nfold, ncol = Nfold)
    }else{
      xsoeji <- matrix(0, nrow = lY %/% Nfold + 1, ncol = Nfold)
      xsoeji[1:lY] <- 1:lY
    }
    
    #Training base models
    train_result <- as.list(numeric(Nfold))
    
    valpr <- matrix(nrow = lY, ncol = length(L))
    colnames(valpr) <- 1:length(L)
    
    for(fold in 1:Nfold){
      
      cat("CV fold", fold, "\n")
      Test <- xsoeji[, fold]
      train_result[[fold]] <- train_basemodel_core(Repeat.parLapply,
                                                   Division,
                                                   L,
                                                   core,
                                                   X.randomised,
                                                   Y.randomised,
                                                   Test)
      
      #Creating explanatory variables for the meta model
      x.test <- X.randomised[Test, ]
      if(Type == "Classification"){
        for(k in 1:length(L))
          valpr[Test, k] <- as.character(predict(train_result[[fold]][[k]], x.test))
      } else {
        for(k in 1:length(L))
          valpr[Test, k] <- predict(train_result[[fold]][[k]], x.test)
      }
    }
  }else{
    
    # Training base models(Random select)
    if(is.null(number) || number <= 0 || number > lY){
      stop("number must be a positive integer less than or equal to the length of Y.narm")
    }
    
    train_result <- as.list(numeric(Nfold))
    
    valpr <- matrix(nrow = lY, ncol = length(L))
    colnames(valpr) <- 1:length(L)
    
    # Randomly select the number of training instances 
    num_train_instances <- sample(lY, number)
    
    for(fold in 1:Nfold){
      
      cat("CV fold", fold, "\n")
      
      # Randomly select training instances
      ORDER <- sample(1:lY, num_train_instances, replace = FALSE)
      
      # Use the rest of the instances as test set
      Test <- setdiff(1:lY, ORDER)
      
      Y.randomised <- Y.narm[ORDER]
      X.randomised <- X.narm[ORDER, ]
      
      # Train the base models
      train_result[[fold]] <- train_basemodel_core(Repeat.parLapply,
                                                   Division,
                                                   L,
                                                   core,
                                                   X.randomised,
                                                   Y.randomised,
                                                   Test)
      
      #Creating explanatory variables for the meta model
      x.test <- X.narm[Test, ]
      if(Type == "Classification"){
        for(k in 1:length(L))
          valpr[Test, k] <- as.character(predict(train_result[[fold]][[k]], x.test))
      } else {
        for(k in 1:length(L))
          valpr[Test, k] <- predict(train_result[[fold]][[k]], x.test)
      }
    }
  }
  
  colnames(valpr) <- 1:length(L)
  
  #Output training results
  basemodel_train_result <- list(train_result = train_result,
                                 no_base = length(L),
                                 valpr = valpr,
                                 Y.randomised = Y.randomised,
                                 Order = ORDER,
                                 Type = Type,
                                 Nfold = Nfold
  )
  basemodel_train_result
}
