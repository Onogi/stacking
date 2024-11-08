stacking_train <- function(X, Y, Nfold, Method, Metamodel, TrainEachFold = FALSE, core = 1, cross_validation = FALSE, use_X = FALSE, num_sample = 10, proportion = 0.8) {

  base <- train_basemodel(X, Y, Nfold, Method, core, cross_validation, num_sample, proportion)

  which_to_use <- 1:base$no_base

  meta <- train_metamodel(base, which_to_use, Metamodel, TrainEachFold, use_X)

  stacking_train_result<-list(base = base,
                              meta = meta
  )

  stacking_train_result
}
