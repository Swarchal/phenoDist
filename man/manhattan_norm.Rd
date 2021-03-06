% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manhattan.R
\name{manhattan_norm}
\alias{manhattan_norm}
\title{Normalised Manhattan distance}
\usage{
manhattan_norm(x, y)
}
\arguments{
\item{x}{numeric vector}

\item{y}{numeric vector}
}
\value{
numeric distance metric
}
\description{
Manhattan distance normalised by the length of the vectors. This is the sum
of the absolute difference between two vectors divided by the length of the
vectors. i.e \code{a <- c(1,2,3); b <- c(3,2,1) ; manhattan(a, b)/3}
}
\examples{
a <- c(1,2,3,4,2,13,2)
b <- c(1,2,4,5,3,23,0)

manhattan_norm(a, b)

}

