\name{train_metamodel}
\alias{train_metamodel}

\description{Training a meta model of stacking
}
\usage{
train_metamodel(basemodel_train_result, which_to_use, Metamodel)
}
\arguments{
  \item{basemodel_train_result}{The list output by train_basemodel}
  \item{which_to_use}{A vector of integers between 1 and L where L is the number of base models. These integers specify the base models used for training the meta model.}
  \item{Metamodel}{A strings specifying the meta learner}
}
\details{
Stacking by this package consists of the following 2 steps. (1) Nfold cross-validation is conducted with each base learner.(2) Using the predicted values of each learner as the explanatory variables, the meta learner is trained. This function conducts step (2). Step (1) is conducted by train_basemodels. Another function stacking_train conducts both steps at once by calling these functions (train_basemodel and train_metamodel).

Meta learners can be chosen from the methods implemented in caret. The choosable methods can be seen at https://topepo.github.io/caret/available-models.html or using names(getModelInfo()) after loading caret.
}
\value{
A list containing the following elements is output.
\item{train_result}{A list containing the training results of the meta model, which is the list output by train function of caret}
\item{which_to_use}{which_to_use given as the argument}
}
\author{
Taichi Nukui, Akio Onogi
}
\seealso{
stacking_train, train_basemodel
}
\examples{
#training

#column names are required by caret

N1<- 100 #Number of samples
P <- 100 #Number of explanatory variables

#Create X of training data
X <- matrix(rnorm(N1 * P), nrow = N1, ncol = P)
colnames(X1) <- 1:P
#The first 10 variables have effects on Y
#Add noise by rnorm
Y <- rowSums(X1[, 1:10]) + rnorm(N1)

Y<-Y
X<-X
Nfold<-10
Method<-c("glmnet","pls","pcr")
Metamodel<-c("lm")
core<-3

stacking_train_result <- stacking_train(Y,X,Nfold,Method,Metamodel,core)

#prediction

#Test data
N2 <- 100 #Number of samples

newX <- matrix(rnorm(N2 * P), nrow = N2, ncol = P)
colnames(newX) <- 1:P

stacking_predict(newX,stacking_train_result)

}
