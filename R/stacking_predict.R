stacking_predict <- function(newX, stacking_train_result){

  library(caret)
  
  mm <- stacking_train_result$meta$train_result
  tr <- stacking_train_result$base$train_result
  lX <- nrow(newX)
  lMt <- length(tr[[1]])
  Nf <- length(tr)

  colnames(newX) <- 1:ncol(newX)
  PV <- matrix(0, nrow = lX, ncol = lMt)
  for(k in 1:Nf){
    for(j in 1:lMt){
      PV[, j] <- PV[, j] + predict(tr[[k]][[j]], newX)
    }
  }

  mPV <- PV/Nf
  colnames(mPV) <- 1:lMt
  result <- as.numeric(predict(mm, mPV[, stacking_train_result$meta$which_to_use, drop = FALSE]))
  
  result
}
