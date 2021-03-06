% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fold_180.R
\name{fold_180}
\alias{fold_180}
\title{Constrains numbers to 180}
\usage{
fold_180(x)
}
\arguments{
\item{x}{Vector of angles (in degrees)}
}
\value{
Numerical vector
}
\description{
Limits a numerical vector to 180. If an element in the vector is greater
 than 180 then the amount it is greater than 180 by is subtracted from it.
 Useful for angles between two vectors, if the angle between two vectors
 is 180 degrees this is the maximum difference -- once the angle exceeds
 180 degrees the vectors begin to converge again.
}
\examples{
sample <- seq(1, 360, 1)
fold_180(sample)
}

