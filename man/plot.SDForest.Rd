% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.SDForest}
\alias{plot.SDForest}
\title{Plot performance of SDForest against number of trees}
\usage{
\method{plot}{SDForest}(x, ...)
}
\arguments{
\item{x}{Fitted object of class \code{SDForest}.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
A ggplot object
}
\description{
This plot helps to analyze whether enough trees were used. If the loss does
not stabilize one can fit another SDForest and merge the two.
}
\seealso{
\code{\link{SDForest}}
set.seed(1)
n <- 10
X <- matrix(rnorm(n * 5), nrow = n)
y <- sign(X[, 1]) * 3 + rnorm(n)
model <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.5, nTree = 500)
plot(model)
}
\author{
Markus Ulmer
}
