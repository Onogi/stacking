stacking_train <- function(X, Y, Method, Metamodel, core = 1, cross_validation = TRUE, use_X = FALSE, TrainEachFold = FALSE, Nfold = 10, num_sample = 10, proportion = 0.8) {

  base <- train_basemodel(X, Y, Method, core, cross_validation, Nfold, num_sample, proportion)

  which_to_use <- 1:base$no_base

  meta <- train_metamodel(X, base, which_to_use, Metamodel, use_X, TrainEachFold)

  stacking_train_result<-list(base = base,
                              meta = meta
  )

  stacking_train_result
}
