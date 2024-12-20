\name{train_metamodel}
\alias{train_metamodel}
\title{Training a meta model based on base models}
\description{
Training a meta model of stacking
}
\usage{
train_metamodel(X, basemodel_train_result, which_to_use, Metamodel, use_X = FALSE, TrainEachFold = FALSE)
}
\arguments{
  \item{X}{An N x P matrix of explanatory variables where N is the number of samples and P is the number of variables. Column names are required by caret.}
  \item{basemodel_train_result}{The list output by train_basemodel}
  \item{which_to_use}{A vector of integers between 1 and L where L is the number of base models. These integers specify the base models used for training the meta model.}
  \item{Metamodel}{A strings specifying the meta learner}
  \item{use_X}{A logical indicating whether the meta-learner uses the original features X along with the base model predictions. If TRUE, it uses both X and the predictions; if FALSE, it uses only the predictions.}
  \item{TrainEachFold}{A logical indicating whether the meta learner learns using the predicted values of the base models at each cross-validation fold/random sample or not. If TRUE, the meta learners learns Nfold times using the values predicted by the base models at each fold/sample. If FALSE, the meta learner learns once by pooling the predicted values of the base models of all folds/samples.}
}
\details{
Stacking by this package consists of the following 2 steps.

(1) Each base learner is trained. The training method can be chosen based on the cross_validation argument:
If cross_validation is TRUE: The function performs Nfold cross-validation for each base learner.
If cross_validation is FALSE: The function trains each base learner using random sampling. The number of samples (num_sample) or the proportion of the data (proportion) can be specified to control the selection process.
(2) Using the predicted values of each learner as the explanatory variables, the meta learner is trained.
Steps (1) and (2) are conducted by train_basemodel and train_metamodel, respectively. 
Another function stacking_train conducts both steps at once by calling these functions (train_basemodel and train_metamodel).\cr

Training of the meta learner can be modified by two arguments, TrainEachFold and use_X.
TrainEachFold specifies whether the meta learner is trained for each fold or random sample individually, or once by pooling or combining all predicted values.
use_X specifies whether the meta learner is trained with both the original features X and the base model predictions, or with only the base model predictions.

Meta learners can be chosen from the methods implemented in caret. The choosable methods can be seen at https://topepo.github.io/caret/available-models.html or using names(getModelInfo()) after loading caret.
}
\value{
A list containing the following elements is output.
\item{train_result}{A list containing the training results of the meta model, which is the list output by train function of caret. When TrainEachFold is TRUE, the length of list is Nfold/num_sample because the meta learner is trained Nfold/num_sample times.}
\item{which_to_use}{which_to_use given as the argument}
\item{cross_validation}{A logical to specify whether to perform cross-validation. Set to TRUE to enable cross-validation or to FALSE to perform random sampling.}
\item{use_X}{use_X}
\item{TrainEachFold}{TrainEachFold}
}
\author{
Taichi Nukui, Tomohiro Ishibashi, Akio Onogi
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
base <- train_basemodel(X = X1, Y = Y1, Method = Method, core = 2, cross_validation = TRUE, Nfold = 5)

#Training of a meta learner
meta <- train_metamodel(base, which_to_use = 1:4, Metamodel = "lm", use_X = FALSE, TrainEachFold = TRUE)

#Combine both results
stacking_train_result <- list(base = base, meta = meta)
#=>this list should have elements named as base and meta

#Prediction
result <- stacking_predict(newX = X2, stacking_train_result)
plot(Y2, result)

#Training using stacking_train
stacking_train_result <- stacking_train(X = X1,
                                        Y = Y1,
                                        Method = Method,
                                        Metamodel = "lm",
                                        core = 2,
                                        cross_validation = TRUE, 
                                        use_X = FALSE, 
                                        TrainEachFold = TRUE, 
                                        Nfold = 5)

#For random sampling, set cross_validation = FALSE and specify the number of samples and the sampling proportion using num_sample and proportion, respectively.
#To include the original features X when training the meta-model, set use_X = TRUE.
#The following code reflects the changes made to the relevant arguments.
stacking_train_result <- stacking_train(X = X1,
                                        Y = Y1,
                                        Method = Method,
                                        Metamodel = "lm",
                                        core = 2,
                                        cross_validation = FALSE, 
                                        use_X = TRUE, 
                                        TrainEachFold = TRUE,
                                        num_sample = 5, 
                                        proportion = 0.8)

#Prediction
result <- stacking_predict(newX = X2, stacking_train_result)
plot(Y2, result)
}
}
