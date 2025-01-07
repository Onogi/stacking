\name{stacking_predict}
\alias{stacking_predict}
\title{Predict for new data}
\description{Return predicted values for newX based on training results of stacking.
}
\usage{
stacking_predict(newX, stacking_train_result)
}
\arguments{
  \item{newX}{An N x P matrix of explanatory variables of new data where N is the number of samples and P is the number of explanatory variables. Note that the order of explanatory variables should be the same as those for training. Column names of newX are ignored.}
  \item{stacking_train_result}{A list output by stacking_train. When train_basemodel and train_metamodel are directly used, a list combining each output should be created and given as stacking_train_result. See examples for this operation.}
}
\details{
Prediction processes of this package are as follows. First, newX is given to all base models. As a result, each base learner returns Nfold/num_sample predicted values where Nfold/num_sample is an argument of stacking_train. Then the predicted values are averaged for each learner. Giving these averaged values as the explanatory variables of the meta model, final predicted values are output.
}
\value{
\item{result}{Vector of predicted values. When TrainEachFold of stacking_train or train_metamodel is TRUE (i.e., stacking_train_result$meta$TrainEachFold is TRUE), the values are the averages of the values predicted from the meta models trained for each cross-validation fold, and for random sampling, the values are the averages of the values predicted from the meta models trained for each random sampling iteration. In the case of classification, the probabilities of each category are returned.}
}
\author{Taichi Nukui, Tomohiro Ishibashi, Akio Onogi
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
#=>This specifies 5 base learners.
##1. glmnet with alpha = 0.5 and lambda = 0.1
##2. glmnet with alpha = 0.5 and lambda = 1
##3. glmnet with alpha = 0.8 and lambda = 0.1
##4. glmnet with alpha = 0.8 and lambda = 1
##5. pls with ncomp = 5

#Training
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
#When use_X is TRUE, simple linear regressions cannot be used as the meta learner because of rank deficient.
#The following code reflects the changes made to the relevant arguments.
stacking_train_result <- stacking_train(X = X1,
                                        Y = Y1,
                                        Method = Method,
                                        Metamodel = "glmnet",
                                        core = 2,
                                        cross_validation = FALSE, 
                                        use_X = TRUE, 
                                        TrainEachFold = TRUE,
                                        num_sample = 5, 
                                        proportion = 0.8)

#Prediction
result <- stacking_predict(newX = X2, stacking_train_result)
plot(Y2, result)

#Training using train_basemodel and train_metamodel
base <- train_basemodel(X = X1, Y = Y1, Method = Method, core = 2, cross_validation = TRUE, Nfold = 5)
meta <- train_metamodel(X, base, which_to_use = 1:5, Metamodel = "lm", use_X = FALSE, TrainEachFold = TRUE)
stacking_train_result <- list(base = base, meta = meta)
#=>The list should have elements named as base and meta to be used in stacking_predict

#Prediction
result <- stacking_predict(newX = X2, stacking_train_result)
plot(Y2, result)

}
