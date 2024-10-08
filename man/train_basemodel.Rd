\name{train_basemodel}
\alias{train_basemodel}
\title{Training base models}
\description{Training base models of stacking. This function internally calls train_basemodel_core.
}
\usage{
train_basemodel(X, Y, Nfold, Method, core = 1)
}
\arguments{
  \item{X}{An N x P matrix of explanatory variables where N is the number of samples and P is the number of variables. Column names are required by caret.}
  \item{Y}{A length N Vector of objective variables. Use a factor for classification.}
  \item{Nfold}{Number of folds for cross-validation. This cross-validation is required for training.}
  \item{Method}{A list specifying base learners. Each element of the list is a data.frame that contains hyperparameter values of base learners. The names of the list elements specifies the base learners and are passed to caret functions. See details and examples}
  \item{core}{Number of cores for parallel processing}
}
\details{
Stacking by this package consists of the following 2 steps. (1) Nfold cross-validation is conducted with each base learner.(2) Using the predicted values of each learner as the explanatory variables, the meta learner is trained. This function conducts step (1). Step (2) is conducted by train_metamodel. Another function stacking_train conducts both steps at once by calling these functions (train_basemodel and train_metamodel).

Base learners are specified by Method. For example,\cr
Method = list(glmnet = data.frame(alpha = 0, lambda = 5), pls = data.frame(ncomp = 10))\cr
indicating that the first base learner is glmnet and the second is pls with corresponding hyperparameters.

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
\item{train_result}{A list containing the training results of the base models. The length of this list is the same as Nfold, and each element is a list of which length is the same as the number of base models. These elements are the lists output by train function of caret, but the element "trainingData" is removed to save memory.}
\item{no_base}{Number of base models.}
\item{valpr}{Predicted values of base models obtained in cross-validation. Used as explanatory variables for the meta learner.}
\item{Y.randomised}{Y ans X are randomized when cross-validation. Randomized Y is output to enable evaluation of prediction accuracy}
\item{Order}{Order in randomization.}
\item{Type}{Type of task (regression or classification).}
\item{Nfold}{Number of cross-validation folds}
}
\author{
Taichi Nukui, Akio Onogi
}
\seealso{
stacking_train, train_metamodel
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
#=>This specifies 4 base learners.
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
