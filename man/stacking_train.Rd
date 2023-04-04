\name{stacking_train}
\alias{stacking_train}

\description{Training base and meta models of stacking, an ensemble learning approach. Base and meta learners can be chosen from supervised methods implemented in caret.
}
\usage{
stacking_train(X, Y, Nfold, Method, Metamodel, core)
}
\arguments{
  \item{X}{An N x P matrix of explanatory variables where N is the number of samples and P is the number of variables}
  \item{Y}{A length N Vector of objective variables}
  \item{Nfold}{Number of folds for cross-validation}
  \item{Method}{A list specifying base learners. Each element of the list is a data.frame that contains hyperparameter values of base learners. The names of the list elements specifies the base learners. See details}
  \item{Metamodel}{A strings specifying the meta learner}
  \item{core}{Number of cores for parallel processing}
}
\details{
Stacking by this function consists of the following 2 steps. (1) Nfold cross-validation is conducted with each base learner.(2) Using the predicted values of each learner as the explanatory variables, the meta learner is trained. This function conducts steps (1) and (2) at once by calling train_basemodel and train_metamodel, respectively. But users can conduct these steps separately by directly using these functions.

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
  
#preparing of hyperparameter (argument "Method")
#Use a portion of the training data to estimate the hyperparameters to be used in stacking_train

a <- c(1:50)
hyp_est_glmnet <- train( X[a,], Y[a], method = glmnet)
hyp_est_hlmnet[[]]

#preparing list of dataframe
Method <- list( glmnet = dataframe( alpha = c(0 , 0.5, 1 ) ,lambda = c(0 , 0.5 ,10 )
                 pls = dataframe( ncomp = c(                  
   
  
  
  
stacking_train_result <- stacking_train(Y,X,Nfold,Method,Metamodel,core)

#prediction

#Test data
N2 <- 100 #Number of samples

newX <- matrix(rnorm(N2 * P), nrow = N2, ncol = P)
colnames(newX) <- 1:P

stacking_predict(newX,stacking_train_result)

}
