% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findFPCenter.R
\name{findFPCenter}
\alias{findFPCenter}
\title{findFPCenter finds the i j coordinates of the center of the firing pin}
\usage{
findFPCenter(image)
}
\arguments{
\item{image}{is the original 2d matrix with depth values representing the 3d image}
}
\value{
an (i,j) coordinate representing the center of the firing pin
}
\description{
findFPCenter finds the i j coordinates of the center of the firing pin
}
\examples{
center <- findFPCenter(matrix(c(1,2,3,4), nrow = 2))

}

