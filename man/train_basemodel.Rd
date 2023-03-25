\name{train_basemodel}
\alias{train_basemodel}

\description{Training base models of stacking
}
\usage{
train_basemodel(X, Y, Nfold, Method, core)
}
\arguments{
  \item{X}{An N x P matrix of explanatory variables where N is the number of samples and P is the number of variables}
  \item{Y}{A length N Vector of objective variables}
  \item{Nfold}{Number of folds for cross-validation}
  \item{Method}{A list specifying base learners. Each element of the list is a data.frame that contains hyperparameter values of base learners. The names of the list elements specifies the base learners. See details}
  \item{core}{Number of cores for parallel processing}
}
\details{
Stacking by this package consists of the following 2 steps. (1) Nfold cross-validation is conducted with each base learner.(2) Using the predicted values of each learner as the explanatory variables, the meta learner is trained. This function conducts step (1). Step (2) is conducted by train_metamodel. Another function stacking_train conducts both steps at once by calling these functions (train_basemodel and train_metamodel).

The base learners are specified by Method. For example,
  Method = list(glmnet = data.frame(alpha = 0, lambda = 5), pls = data.frame(ncomp = 10))
indicating that the first base learner is glmnet and the second is pls with corresponding hyperparameters.
When the data.frame has multiple rows as
  Method = list(glmnet = data.frame(alpha = c(0, 1), lambda = c(5, 10)), pls = data.frame(ncomp = 10))
all combinations of hyperparameter values are automatically created as
  [alpha, lambda] = [0, 5], [0, 10], [1, 5], [1, 10]
Thus, in total 5 base learners (4 glmnet and 1 pls) are created.
When the number of values differ among hyperparameters, use NA as
  Method = list(glmnet = data.frame(alpha = c(0, 0.5, 1), lambda = c(5, 10, NA)), pls = data.frame(ncomp = 10))
resulting in 6 combinations of
  [alpha, lambda] = [0, 5], [0, 10], [0.5, 5], [0.5, 10], [1, 5], [1, 10]
When a hyperparameter includes only NA as
  Method = list(glmnet = data.frame(alpha = c(0, 0.5, 1), lambda = c(NA, NA, NA)), pls = data.frame(ncomp = NA))
lambda of glmnet and ncomp of pls are automatically tuned by caret. However, it is notable that tuning is conducted assuming that all hyperparameters are unknown, and thus, the tuned lambea in the above example is not the value tuned under the given alpha values (0, 0.5, or 1).

The base learners can be chosen from the methods implemented in caret. The choosable methods can be seen at https://topepo.github.io/caret/available-models.html or using names(getModelInfo()) after loading caret.
}
\value{
A list containing the following elements is output.
\item{train_result}{A list containing the training results of the base models. The length of this list is the same as Nfold, and each element is a list of which length is the same as the number of base models. The elements are the lists output by train function of caret}
\item{no_base}{Number of base models.}
\item{valpr}{Predicted values of base models obtained in cross-validation. Used as explanatory variables for the meta learner.}
\item{Y.randomised}{Y ans X are randomized when cross-validation. Randomized Y is output to enable evaluation of prediction accuracy}
\item{Order}{Order in randomization.}
}
\author{
Taichi Nukui, Akio Onogi
}
\seealso{
stacking_train, train_metamodel
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
