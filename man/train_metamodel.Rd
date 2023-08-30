\name{train_metamodel}
\alias{train_metamodel}
\title{Training a meta model based on base models}
\description{
Training a meta model of stacking
}
\usage{
train_metamodel(basemodel_train_result, which_to_use, Metamodel, TrainEachFold = FALSE)
}
\arguments{
  \item{basemodel_train_result}{The list output by train_basemodel}
  \item{which_to_use}{A vector of integers between 1 and L where L is the number of base models. These integers specify the base models used for training the meta model.}
  \item{Metamodel}{A strings specifying the meta learner}
  \item{TrainEachFold}{A logical indicating whether the meta learner learns using the predicted values of the base models at each cross-validation fold or not. If TRUE, the meta learners learns Nfold times using the values predicted by the base models at each fold. If FALSE, the meta learner learns once by pooling the predicted values of the base models of all folds.}
}
\details{
Stacking by this package consists of the following 2 steps. (1) Nfold cross-validation is conducted with each base learner.(2) Using the predicted values of each learner as the explanatory variables, the meta learner is trained. This function conducts step (2). Step (1) is conducted by train_basemodel. Another function stacking_train conducts both steps at once by calling these functions (train_basemodel and train_metamodel).\cr

In the step (2), there are two options. One is to train the meta learner Nfold times using the predicted values returned by the base models for each fold. The other is to train the meta learner once pooling the predicted values by the base models across folds. TrainEachModel swiches these options.\cr

Meta learners can be chosen from the methods implemented in caret. The choosable methods can be seen at https://topepo.github.io/caret/available-models.html or using names(getModelInfo()) after loading caret.
}
\value{
A list containing the following elements is output.
\item{train_result}{A list containing the training results of the meta model, which is the list output by train function of caret. When TrainEachFold is TRUE, the length of list is Nfold because the meta learner is trained Nfold times.}
\item{which_to_use}{which_to_use given as the argument}
\item{TrainEachFold}{TrainEachFold}
}
\author{
Taichi Nukui, Akio Onogi
}
\seealso{
stacking_train, train_basemodel
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
Method <- list(glmnet = data.frame(alpha = c(0, 0.5, 1), lambda = rep(NA, 3)),
               pls = data.frame(ncomp = 5))
#=>This specifies four base learners.
##1. glmnet with alpha = 0 and lambda tuned
##2. glmnet with alpha = 0.5 and lambda tuned
##3. glmnet with alpha = 1 and lambda tuned
##4. pls with ncomp = 5
\donttest{
#Training of base learners
base <- train_basemodel(X = X1, Y = Y1, Nfold = 5, Method = Method, core = 2)

#Training of a meta learner
meta <- train_metamodel(base, which_to_use = 1:4, Metamodel = "lm")

#Combine both results
stacking_train_result <- list(base = base, meta = meta)
#=>this list should have elements named as base and meta

#Prediction
result <- stacking_predict(newX = X2, stacking_train_result)
plot(Y2, result)

#Training using stacking_train
stacking_train_result <- stacking_train(X = X1,
                                        Y = Y1,
                                        Nfold = 5,
                                        Method = Method,
                                        Metamodel = "lm",
                                        core = 2)

#Prediction
result <- stacking_predict(newX = X2, stacking_train_result)
plot(Y2, result)
}
}
