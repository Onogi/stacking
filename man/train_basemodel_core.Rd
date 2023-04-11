\name{train_basemodel_core}
\alias{train_basemodel_core}

\description{Training base models of stacking. This function is called by train_basemodel and designed for the internal use of train_basemodel.
}
\usage{
train_basemodel_core(repeat.parLapply, division, l, core, x, y, exclude)
}
\arguments{
  \item{repeat.parLapply}{A scalar indicating the number of repeats of parallel computation. If the number of base models is 10 and 5 cores are used for computation, repeat.parLapply is 2.}
  \item{division}{A matrix of which the number of columns is equal to repeat.parLapply. The elements are integers indicating the base models. For example, division[, 1] indicates the base models trained in the first calculation round.}
  \item{l}{A nested list indicating the training method and hyperparameters. The length is the number of base models. Each element is a list consisting of two elements, method and hyp, which are strings indicating the training method and a data frame including hyperparameter values, respectively. The number of columns of the data frame is the number of hyperparameters of the method, and the hyperparameter names should be specified as the column names.}
  \item{core}{Number of cores for parallel processing}
  \item{x}{An N x P matrix of explanatory variables where N is the number of samples and P is the number of variables}
  \item{y}{A length N Vector of objective variables}
  \item{exclude}{A vector of integers indicating the samples excluded from training as testing data} 
}
\details{
This function is designed for the internal use and not for the users. Thus, detaled usages are not provided.
}
\value{
A list containing the training results of base models.
}
\author{
Taichi Nukui, Akio Onogi
}
\seealso{
train_basemodel
}
