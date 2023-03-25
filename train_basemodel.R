train_basemodel <- function(X, Y, Nfold, Method, core){
  
  library(snow)
  library(caret)
  lM <- length(Method)
  
  #Checking the names and numbers of hyperparameters for each method
  check <- numeric(length = lM)
  
  for(m in 1:lM){
    if(!setequal(colnames(Method[[m]]), modelLookup(names(Method)[m])$parameter)){
      warning("Hyperparameters of ", names(Method)[[m]], " is incorrect")
      check[m] <- 1
    }
  }
  if(any(check>0)){stop("Please confirm the numbers and/or names of hyperparameters of the above methods")}
  
  method <- names(Method)
  
  #Specify hyperparameter values when Null is given
  for(m in 1:lM){
      Method[[m]]
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
  }
  
  #Removing NA
  Y.narm <- Y[!is.na(Y)]
  X.narm <- X[!is.na(Y), ]
  lY <- length(Y.narm)
  
  #Dividing data for cross-validation
  ORDER <- sample(1:lY, lY, replace=FALSE)
  Y.randomised <- Y.narm[ORDER]
  X.randomised <- X.narm[ORDER, ]
  colnames(X.randomised) <- 1:ncol(X.randomised)
  
  if(lY%%Nfold == 0){
    xsoeji <- matrix(1:lY, nrow = lY %/% Nfold, ncol = Nfold)
  }else{
    xsoeji <- matrix(0, nrow = lY %/% Nfold + 1, ncol = Nfold)
    xsoeji[1:lY] <- 1:lY
  }
  
  #Training base models
  train_result <- as.list(numeric(Nfold))
  metamodel <- as.list(numeric(1))
  
  valpr <- matrix(nrow = lY, ncol = length(L))
  colnames(valpr) <- 1:length(L)
  
  for(i in 1:Nfold){
    Test <- xsoeji[, i]
    X.test <- X.randomised[Test, ]
    Y.test <- Y.randomised[Test]
    X.train <- X.randomised[-Test, ]
    Y.train <- Y.randomised[-Test]
    
    cl <- makeCluster(core, type="SOCK")
    clusterExport(cl, c("X.train",
                        "Y.train",
                        "train"), envir = environment())

    train_result[i] <- list(NULL)
    for(rp in 1:Repeat.parLapply){
      
      train_result[[i]]<-c(train_result[[i]],
                           parLapply(cl, L[Division[, rp]],
                                   function(m){
                                     train(X.train, Y.train, method = m$method, tuneGrid = m$hyp)
                                   })
      )
    }
    stopCluster(cl)
    
    #Creating explanatory variables for the meta model
    for(k in 1:length(L))
      valpr[Test, k] <- predict(train_result[[i]][[k]], X.test)
  }
  colnames(valpr) <- 1:length(L)

  #Output training results
  basemodel_train_result <- list(train_result = train_result,
                                 no_base = length(L),
                                 valpr = valpr,
                                 Y.randomised = Y.randomised,
                                 Order = ORDER
  )
  basemodel_train_result
}
