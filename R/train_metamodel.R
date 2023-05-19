train_metamodel <- function(basemodel_train_result, which_to_use, Metamodel){

  nb <- basemodel_train_result$no_base

  if(is.null(which_to_use)) which_to_use <- 1:nb

  #checking which_to_use
  if(length(which_to_use) > nb)
    stop("Length of which_to_use must not exceed ", nb)

  if(0 < sum(which_to_use > nb))
    stop("which_to_use contains larger values than the number of base models (", nb,")")

  valpr <- basemodel_train_result$valpr[, which_to_use, drop = FALSE]

  if(basemodel_train_result$Type == "Classification"){
    Category <- sort(unique(basemodel_train_result$Y.randomised))
    #Add all categories to each base model output (to make model.matrix outputs same as prediction)
    Addline <- matrix(Category, nrow = length(Category), ncol = nb)
    valpr <- rbind(valpr, Addline)
    valpr <- data.frame(valpr)
    valpr <- model.matrix(~., data = valpr)
    #Remove added lines
    valpr <- valpr[-c((nrow(valpr) - length(Category) + 1):nrow(valpr)), ]
  }
  metamodel <- train(valpr, basemodel_train_result$Y.randomised, method = "glmnet")

  metamodel_train_result <- list(train_result = metamodel,
                                 which_to_use = which_to_use)
  metamodel_train_result
}
