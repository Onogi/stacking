stacking_train <- function(X, Y, Method, Metamodel, core = 1, cross_validation = FALSE, use_X = FALSE, TrainEachFold = FALSE, Nfold = 10, num_sample = 10, proportion = 0.8) {
  ################################################################################
  #train_basemodel同様にNfoldの位置を変更して初期値を与える。以下もNfold1の位置が要修正
  ################################################################################
  base <- train_basemodel(X, Y, Method, core, cross_validation, Nfold, num_sample, proportion)

  which_to_use <- 1:base$no_base

  meta <- train_metamodel(base, which_to_use, Metamodel, TrainEachFold, use_X)

  stacking_train_result<-list(base = base,
                              meta = meta
  )

  stacking_train_result
}
