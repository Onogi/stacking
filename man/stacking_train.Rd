\name{stacking_train}
\alias{stacking_train}
\title{Training base and meta models}
\description{Training base and meta learners of stacking (an ensemble learning approach). The base and meta learners can be chosen from supervised methods implemented in caret. This function internally calls train_basemodel and train_metamodel. Packages caret, parallel, snow, and packages for base and meta learners should be installed.
}
\usage{
stacking_train(X, Y, Nfold, Method, Metamodel, TrainEachFold = FALSE, core = 1)
}
\arguments{
  \item{X}{An N x P matrix of explanatory variables where N is the number of samples and P is the number of variables. Column names are required by caret.}
  \item{Y}{A length N Vector of objective variables. Use a factor for classification.}
  \item{Nfold}{Number of folds for cross-validation. This cross-validation is required for training.}
  \item{Method}{A list specifying base learners. Each element of the list is a data.frame that contains hyperparameter values of base learners. The names of the list elements specifies the base learners and are passed to caret functions. See details and examples}
  \item{Metamodel}{A strings specifying the meta learner. This strings is passed to caret.}
  \item{TrainEachFold}{A logical indicating whether the meta learner learns using the predicted values of the base models at each cross-validation fold or not. If TRUE, the meta learners learns Nfold times using the values predicted by the base models at each fold. If FALSE, the meta learner learns once by pooling the predicted values of the base models of all folds.}
  \item{core}{Number of cores for parallel processing}
}
\details{
Stacking by this function consists of the following 2 steps. (1) Nfold cross-validation is conducted with each base learner.(2) Using the predicted values of each learner as the explanatory variables, the meta learner is trained. This function conducts steps (1) and (2) at once by calling train_basemodel and train_metamodel, respectively. But users can conduct these steps separately by directly using these functions.\cr

In the step (2), there are two options. One is to train the meta learner Nfold times using the predicted values returned by the base models for each fold. The other is to train the meta learner once pooling the predicted values by the base models across folds. TrainEachModel swiches these options.\cr

Base learners are specified by Method. For example,\cr
Method = list(glmnet = data.frame(alpha = 0, lambda = 5), pls = data.frame(ncomp = 10))\cr
indicating that the first base learner is glmnet and the second is pls with the corresponding hyperparameters.

When the data.frames have multiple rows as\cr
Method = list(glmnet = data.frame(alpha = c(0, 1), lambda = c(5, 10)))\cr
All combinations of hyperparameter values are automatically created as\cr
[alpha, lambda] = [0, 5], [0, 10], [1, 5], [1, 10]\cr
Thus, in total 5 base learners (4 glmnet and 1 pls) are created.

When the number of candidate values differ among hyperparameters, use NA as\cr
Method = list(glmnet = data.frame(alpha = c(0, 0.5, 1), lambda = c(5, 10, NA)))\cr
resulting in 6 combinations of\cr
[alpha, lambda] = [0, 5], [0, 10], [0.5, 5], [0.5, 10], [1, 5], [1, 10]

When a hyperparameter includes only NA as\cr
Method = list(glmnet = data.frame(alpha = c(0, 0.5, 1), lambda = c(NA, NA, NA)), pls = data.frame(ncomp = NA))\cr
lambda of glmnet and ncomp of pls are automatically tuned by caret. However, it is notable that tuning is conducted assuming that all hyperparameters are unknown, and thus, the tuned lambea in the above example is not the value tuned under the given alpha values (0, 0.5, or 1).

Hyperparameters of meta learners are automatically tuned by caret.

The base and meta learners can be chosen from the methods implemented in caret. The choosable methods can be seen at https://topepo.github.io/caret/available-models.html or using names(getModelInfo()) after loading caret.
}
\value{
A list containing the following elements is output.
\item{base}{A list output by train_basemodel. See value of train_basemodel for the details}
\item{meta}{A list output by train_metamodel. See value of train_metamodel for the details}
}
\author{
Taichi Nukui, Akio Onogi
}
\seealso{
train_basemodel, train_metamodel
}
\examples{
#Create a toy example
##Number of training samples
N1 <- 100

##Number of explanatory variables
P <- 200

##Create X of training data
X1 <- matrix(rnorm(N1 * P), nrow = N1, ncol = P)
colnames(X1) <- 1:P#column names are required by caret

##Assume that the first 10 variables have effects on Y
##Then add noise with rnorm
Y1 <- rowSums(X1[, 1:10]) + rnorm(N1)

##Test data
N2 <- 100
X2 <- matrix(rnorm(N2 * P), nrow = N2, ncol = P)
colnames(X2) <- 1:P#Ignored (not required)
Y2 <- rowSums(X2[, 1:10])

#Specify base learners
Method <- list(glmnet = data.frame(alpha = c(0.5, 0.8), lambda = c(0.1, 1)),
               pls = data.frame(ncomp = 5))
#=>This specifies five base learners.
##1. glmnet with alpha = 0.5 and lambda = 0.1
##2. glmnet with alpha = 0.5 and lambda = 1
##3. glmnet with alpha = 0.8 and lambda = 0.1
##4. glmnet with alpha = 0.8 and lambda = 1
##5. pls with ncomp = 5
\donttest{
stacking_train_result <- stacking_train(X = X1,
                                        Y = Y1,
                                        Nfold = 5,
                                        Method = Method,
                                        Metamodel = "lm",
                                        core = 3)

#Prediction
result <- stacking_predict(newX = X2, stacking_train_result)
plot(Y2, result)

#Training using train_basemodel and train_metamodel
base <- train_basemodel(X = X1, Y = Y1, Nfold = 5, Method = Method, core = 3)
meta <- train_metamodel(base, which_to_use = 1:5, Metamodel = "lm")
stacking_train_result <- list(base = base, meta = meta)
#=>this list should have elements named as base and meta

#Prediction
result <- stacking_predict(newX = X2, stacking_train_result)
plot(Y2, result)

#In the simulations of the reference paper (Nukui and Onogi 2023),
#we use 48 base learners as
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
#mtry of ranger and ncomp of pls should be arranged according to data size.

#In the classification example of the reference paper, for RNA features, we used
Method <- list(ranger = data.frame(mtry = c(10, 100, 500),
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
#svmRadial was replaced by svmPoly
#These base learners may be a good starting point.

}
}
