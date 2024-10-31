stacking_train <- function(X, Y, Nfold, Method, Metamodel, TrainEachFold = FALSE, core = 1){

  base <- train_basemodel(X, Y, Nfold, Method, core)

  which_to_use <- 1:base$no_base

  meta <- train_metamodel(base, which_to_use, Metamodel, TrainEachFold, cross_validation)

  stacking_train_result<-list(base = base,
                              meta = meta
  )

  stacking_train_result
}
