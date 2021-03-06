% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/localRemove.R
\name{localRemove}
\alias{localRemove}
\title{localRemove removes the extreme depth values from the image}
\usage{
localRemove(image, windowSize = 120)
}
\arguments{
\item{image}{is the original 2d array of depth values}

\item{windowSize}{is the size of the local region you want to filter
extreme values from}
}
\value{
the same image but with the extreme depth values removed in
local regions whose sizes are determined by windowSize
}
\description{
localRemove removes the extreme depth values from the image
}
\examples{
breechFace <- localRemove(matrix(c(1,2,3,4), nrow = 2))

}

