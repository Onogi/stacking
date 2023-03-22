train_metamodel <- function(basemodel_train_result, which_to_use, Metamodel){

  nb <- basemodel_train_result$no_base
  
  #checking which_to_use
  if(length(which_to_use) > nb)
    stop("Length of which_to_use must not exceed ", nb)

  if(0 < sum(which_to_use>nb))
    stop("which_to_use contains larger values than the number of base models (", nb,")")

  if(is.null(which_to_use)) which_to_use <- 1:nb

  valpr<-basemodel_train_result$valpr[, which_to_use, drop = FALSE]

  library(caret)

  metamodel <- train(valpr, basemodel_train_result$Y.randomised, method = Metamodel)

  metamodel_train_result <- list(train_result = metamodel,
                                 which_to_use = which_to_use)
  metamodel_train_result
}
