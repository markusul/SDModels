% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copy.R
\name{copy.SDForest}
\alias{copy.SDForest}
\alias{copy}
\title{Copy a forest}
\usage{
\method{copy}{SDForest}(object, ...)
}
\arguments{
\item{object}{an SDForest object}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
A copy of the SDForest object
}
\description{
Returns a copy of the forest object.
Might be useful if you want to keep the original forest in comparison to the pruned forest.
}
\examples{

set.seed(1)
X <- matrix(rnorm(10 * 20), nrow = 10)
Y <- rnorm(10)
fit <- SDForest(x = X, y = Y, nTree = 2, cp = 0.5)
fit2 <- copy(fit)
}
\seealso{
\code{\link{prune}}
}
\author{
Markus Ulmer
}
