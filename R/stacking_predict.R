stacking_predict <- function(newX, stacking_train_result){

  if(length(setdiff(c("base", "meta"), names(stacking_train_result))) > 0)
    stop("stacking_train_result requires elements named as base and meta")

  mm <- stacking_train_result$meta$train_result
  tr <- stacking_train_result$base$train_result
  lX <- nrow(newX)
  lMt <- length(tr[[1]])
  Nf <- length(tr)

  colnames(newX) <- 1:ncol(newX)
  if(stacking_train_result$base$Type == "Classification"){
    PV <- array(data = NA, dim = c(lX, lMt, Nf))
    for(k in 1:Nf)
      for(j in 1:lMt)
        PV[, j, k] <- as.character(predict(tr[[k]][[j]], newX))

    mPV <- matrix(NA, nrow = lX, ncol = lMt)
    for(j in 1:lMt){
      for(i in 1:lX){
        v <- table(PV[i, j, ])
        if(sum(v == max(v)) > 1) {
          mPV[i, j] <- sample(names(v)[which(v == max(v))], 1)
        } else {
          mPV[i, j] <- names(v)[which.max(v)]
        }
      }
    }

    Category <- sort(unique(stacking_train_result$base$Y.randomised))
    #Add all categories to each base model output (to make model.matrix outputs same as training)
    Addline <- matrix(Category, nrow = length(Category), ncol = lMt)
    mPV <- rbind(mPV, Addline)
    mPV <- data.frame(mPV[, stacking_train_result$meta$which_to_use, drop = FALSE])
    mPV <- model.matrix(~., data = mPV)
    #Remove added lines
    mPV <- mPV[-c((nrow(mPV) - length(Category) + 1):nrow(mPV)), ]
    result <- as.character(predict(mm, mPV))
  } else {
    #Regression
    PV <- matrix(0, nrow = lX, ncol = lMt)
    for(k in 1:Nf)
      for(j in 1:lMt)
        PV[, j] <- PV[, j] + predict(tr[[k]][[j]], newX)

    mPV <- PV/Nf
    colnames(mPV) <- 1:lMt
    result <- as.numeric(predict(mm, mPV[, stacking_train_result$meta$which_to_use, drop = FALSE]))
  }

  result
}
