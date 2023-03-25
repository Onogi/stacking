\name{stacking_predict}
\alias{stacking_predict}

\description{Return predicted values for newX based on training results of stacking.
}
\usage{
stacking_predict(newX, stacking_train_result)
}
\arguments{
  \item{newX}{An N x P matrix of explanatory variables for new data where N is the number of samples and P is the number of explanatory variables}
  \item{stacking_train_result}{A list output by stacking_train. When train_basemodel and train_metamodel are directly used, a list combining each output should be created and given as stacking_train_result. See details for this operation.}
}
\details{
Stacking implemented by this package predicts as follows. First, newX is given to all base models. As a result, each base learner returns Nfold predicted values. Then the predicted values are averaged for each learner. Giving these averaged values as the explanatory variables to the meta model, the final predicted values are obtained.

When train_basemodel and train_metamodel are directly used, stacking_train_result should be created as follows:
stacking_train_result <- list(base = train_basemodel_result, meta = train_metamodel_result)
Here train_basemodel_result and train_metamodel_result are the lists output by train_basemodel and train_metamodel, respectively. See also examples.
}
\value{
\item{result}{Vector of predicted values}
}
\author{Taichi Nukui, Akio Onogi
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
