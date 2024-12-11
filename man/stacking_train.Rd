\name{stacking_train}
\alias{stacking_train}
\title{Training base and meta models}
\description{Training base and meta learners of stacking (an ensemble learning approach). The base and meta learners can be chosen from supervised methods implemented in caret.
}
\usage{
stacking_train(X, Y, Method, Metamodel, core = 1, cross_validation = FALSE, use_X = FALSE, TrainEachFold = FALSE, Nfold = 10, num_sample = 10, proportion = 0.8)
}
\arguments{
  \item{X}{An N x P matrix of explanatory variables where N is the number of samples and P is the number of variables. Column names are required by caret.}
  \item{Y}{A length N Vector of objective variables. Use a factor for classification.}
　\item{Method}{A list specifying base learners. Each element of the list is a data.frame that contains hyperparameter values of base learners. The names of the list elements specifies the base learners and are passed to caret functions. See details and examples}
  \item{Metamodel}{A strings specifying the meta learner. This strings is passed to caret.}
　\item{core}{Number of cores for parallel processing}
  \item{cross_validation}{A parameter to specify whether to perform cross-validation. Set to TRUE to enable cross-validation or to FALSE to perform random selection.}
　\item{use_X}{A logical indicating whether the meta-learner uses the original features X along with the base model predictions. If TRUE, it uses both X and the predictions; if FALSE, it uses only the predictions.}
　\item{TrainEachFold}{A logical indicating whether the meta learner learns using the predicted values of the base models at each cross-validation fold or not. If TRUE, the meta learners learns Nfold times using the values predicted by the base models at each fold. If FALSE, the meta learner learns once by pooling the predicted values of the base models of all folds.}
  \item{Nfold}{Number of folds for cross-validation. This cross-validation is required for training.}
　\item{num_sample}{The number of samples for random selection, applicable when cross_validation is set to FALSE.}
  \item{proportion}{A parameter specifying the proportion of samples to be selected when cross_validation is set to FALSE.}
}
\details{
Stacking by this package consists of the following 2 steps.

(1) Each base learner is trained. The training method can be chosen based on the cross_validation parameter:
If cross_validation is TRUE: The function performs Nfold cross-validation for each base learner.
If cross_validation is FALSE: The function trains each base learner using random sampling. The number of samples (num_sample) or the proportion of the data (proportion) can be specified to control the selection process.
(2) Using the predicted values of each learner as the explanatory variables, the meta learner is trained.This function conducts step (2).
Step (1) is conducted by train_basemodel. 
Another function stacking_train conducts both steps at once by calling these functions (train_basemodel and train_metamodel).\cr

In the step (2), there are six options. 
If Cross-Validation is Used.One is to train the meta learner Nfold times using the predicted values returned by the base models for each fold.　The other is to train the meta learner once pooling the predicted values by the base models across folds. 
If Random Sampling is Used.One option is to train the meta learner num_sample times, using the predicted values returned by the base models for each random sampling.　The other option is to train the meta learner once by combining the predicted values from all random samplings.
In both cases, the TrainEachFold parameter controls whether the meta learner is trained for each fold or random sample individually, or once by pooling or combining all predicted values.
The use_X parameter controls whether the meta learner is trained with both the original features X and the base model predictions, or with only the base model predictions.

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
                                        Method = Method,
                                        Metamodel = "lm",
                                        core = 3,
                                        cross_validation = TRUE, 
                                        use_X = FALSE, 
                                        TrainEachFold = TRUE, 
                                        Nfold = 5, 
                                        num_sample = 5, 
                                        proportion = 0.8)

#Prediction
result <- stacking_predict(newX = X2, stacking_train_result)
plot(Y2, result)

#Training using train_basemodel and train_metamodel
base <- train_basemodel(X = X1, Y = Y1, Method = Method, core = 3, cross_validation = TRUE, Nfold = 5, num_sample = 5, proportion = 0.8)
meta <- train_metamodel(base, which_to_use = 1:5, Metamodel = "lm", use_X = FALSE, TrainEachFold = TRUE)
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
