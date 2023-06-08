#R scripts for simulation

#Number of samples
N.train <- 1000
N.test <- 1000

#Number of explanatory variables
P <- 200

#Create simulated data
for(sim in 1:20){
  
  #Create X
  X.train <- matrix(rnorm(N.train * P), nrow = N.train, ncol = P)
  X.test <- matrix(rnorm(N.test * P), nrow = N.test, ncol = P)
  colnames(X.train) <- colnames(X.test) <- paste0("X", 1:P)
  
  #The first 20 variables and their interactions have effects on Y
  #Add noise by rnorm
  Y.train <- rowSums(X.train[, 1:20])
  Y.test <- rowSums(X.test[, 1:20])
  for(i in 1:19){
    Y.train <- Y.train + (X.train[, i] * X.train[, i + 1])
    Y.test <- Y.test + (X.test[, i] * X.test[, i + 1])    
  }
  Y.train <- Y.train + rnorm(N.train, 0, sqrt(var(Y.train) * 0.25))
  
  write.csv(X.train, paste0("Sim", sim, ".Xtrain.csv"))
  write.csv(X.test, paste0("Sim", sim, ".Xtest.csv"))
  write.csv(Y.train, paste0("Sim", sim, ".Ytrain.csv"))
  write.csv(Y.test, paste0("Sim", sim, ".Ytest.csv"))
}

#Base and meta learners
Method <- list(ranger = data.frame(mtry = c(10, 100, 200),
                                   splitrule = c("extratrees", NA, NA),
                                   min.node.size = c(1, 5, 10)),
               xgbTree = data.frame(colsample_bytree = c(0.6, 0.8),
                                    subsample = c(0.5, 1),
                                    nrounds = c(50, 150),
                                    max_depth = c(6, NA),
                                    eta = c(0.3, NA),
                                    gamma = c(0, NA),
                                    min_child_weight = c(1, NA)),
               gbm = data.frame(interaction.depth = c(1, 3, 5),
                                n.trees = c(50, 100, 150),
                                shrinkage = c(0.1, NA, NA),
                                n.minobsinnode = c(10, NA, NA)),
               svmPoly = data.frame(C = c(0.25, 0.5, 1),
                                    scale = c(0.001, 0.01, 0.1),
                                    degree = c(1, NA, NA)),
               glmnet = data.frame(alpha = c(1, 0.8, 0.6, 0.4, 0.2, 0),
                                   lambda = rep(NA, 6)),
               pls = data.frame(ncomp = seq(2, 70, 10))
)

Metamethod <- "glmnet"

#Methods compared
Method.compared <- c("glmnet", "ranger", "gbm", "xgbTree", "svmPoly", "pls")

#Prediction
library(stacking)
library(caret)
Prediction1 <- array(0, dim = c(N.test, length(Method.compared) + 1, 20),
                     dimnames = list(1:N.test,
                                     c("stacking", Method.compared),
                                     1:20)
)

for(sim in 1:20){
  
  X.train <- as.matrix(read.csv(paste0("Sim", sim, ".Xtrain.csv"), header = TRUE, row.names = 1))
  X.test <- as.matrix(read.csv(paste0("Sim", sim, ".Xtest.csv"), header = TRUE, row.names = 1))
  Y.train <- unlist(read.csv(paste0("Sim", sim, ".Ytrain.csv"), header = TRUE, row.names = 1))
  
  #stacking
  stacking_train_result <- stacking_train(X.train, Y.train, 5, Method, Metamodel, 16)
  Prediction1[, "stacking", sim] <- stacking_predict(X.test, stacking_train_result)
  rm(stacking_train_result)
  
  #Others
  for(m in Method.compared){
    result <- train(X.train, Y.train, method = m)
    Prediction1[, m, sim] <- predict(result, X.test)
  }
}

#Evaluation
Accuracy1 <- array(0, dim = c(20, length(Method.compared) + 1, 2),
                   dimnames = list(1:20,
                                   c("stacking", Method.compared),
                                   c("Correlation", "RMSE"))
)

for(sim in 1:20){
  
  Y.test <- unlist(read.csv(paste0("Sim", sim, ".Ytest.csv"), header = TRUE, row.names = 1))
  Accuracy1[sim, "stacking", "Correlation"] <- cor(Prediction1[, "stacking", sim], Y.test)
  Accuracy1[sim, "stacking", "RMSE"] <- sqrt(mean((Prediction1[, "stacking", sim] - Y.test)^2))
  for(m in Method.compared){
    Accuracy1[sim, m, "Correlation"] <- cor(Prediction1[, m, sim], Y.test)
    Accuracy1[sim, m, "RMSE"] <- sqrt(mean((Prediction1[, m, sim] - Y.test)^2))
  }
}

