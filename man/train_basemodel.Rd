\name{train_basemodel}
\alias{train_basemodel}
\title{Training base models}
\description{Training base models of stacking. This function internally calls train_basemodel_core.
}
\usage{
train_basemodel(X, Y, Method, core = 1, cross_validation = TRUE, Nfold = 10, num_sample = 10, proportion = 0.8)
}
\arguments{
  \item{X}{An N x P matrix of explanatory variables where N is the number of samples and P is the number of variables. Column names are required by caret.}
  \item{Y}{A length N Vector of objective variables. Use a factor for classification.}
  \item{Method}{A list specifying base learners. Each element of the list is a data.frame that contains hyperparameter values of base learners. The names of the list elements specifies the base learners and are passed to caret functions. See details and examples}
  \item{core}{Number of cores for parallel processing}
  \item{cross_validation}{A parameter to specify whether to perform cross-validation. Set to TRUE to enable cross-validation or to FALSE to perform random sampling.}
  \item{Nfold}{Number of folds for cross-validation. Required when cross_validation is TRUE.}
  \item{num_sample}{The number of samples for random sampling, applicable when cross_validation is set to FALSE.}
  \item{proportion}{A parameter specifying the proportion of samples to be sampled when cross_validation is set to FALSE.}
}
\details{
Stacking by this package consists of the following 2 steps. 
(1) Each base learner is trained.The training method can be chosen based on the cross_validation argument.
If cross_validation is TRUE: The function performs Nfold cross-validation for each base learner.
If cross_validation is FALSE: The function trains each base learner using random sampling. The number of samples (num_sample) or the proportion of the data (proportion) can be specified to control the sampling process.
(2) Using the predicted values of each learner as the explanatory variables, the meta learner is trained.
Steps (1) and (2) are conducted by train_basemodel and train_metamodel, respectively. 
Another function stacking_train conducts both steps at once by calling these functions (train_basemodel and train_metamodel).\cr

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
\item{train_result}{A list containing the training results of the base models. The length of this list is the same as Nfold/num_sample, and each element is a list of which length is the same as the number of base models. These elements are the lists output by train function of caret, but the element "trainingData" is removed to save memory.}
\item{no_base}{Number of base models.}
\item{valpr}{Predicted values of base models obtained in cross-validation/random sampling. Used as explanatory variables for the meta learner.}
\item{Y.randomised}{Y of the test sets of cross-validation/random sampling. Used as the response variable for the meta learner.}
\item{Order}{Indices of Y used in cross-validation/random sampling. The indices were those of Y without NA values.}
\item{Type}{Type of task (regression or classification).}
\item{Nfold}{Number of cross-validation folds.}
\item{num_sample}{Number of samples in random sampling.}
\item{which_valid}{Indices of Y without NA values.}
\item{cross_validation}{Specifies which cross-validation (TRUE) or random sampling (FALSE) was used during training.}
}
\author{
Taichi Nukui, Tomohiro Ishibashi, Akio Onogi
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
base <- train_basemodel(X = X1, Y = Y1, Method = Method, core = 2, cross_validation = TRUE, Nfold = 5)

#Training of a meta learner
meta <- train_metamodel(X, base, which_to_use = 1:4, Metamodel = "lm")

#Combine both results
stacking_train_result <- list(base = base, meta = meta)
#=>The list should have elements named as base and meta to be used in stacking_predict

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
                                        TrainEachFold = FALSE, 
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
                                        TrainEachFold = FALSE,
                                        num_sample = 5, 
                                        proportion = 0.8)

#Prediction
result <- stacking_predict(newX = X2, stacking_train_result)
plot(Y2, result)
}
}
