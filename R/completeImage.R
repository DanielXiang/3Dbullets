#' completeImage takes as input a 2D array of depth values which may contain NA
#' values, and applies completeSeq row wise, and then column wise, and then takes
#' the average of the two resulting outputs.
#'
#' @param image is the 2D array of depth values
#' @return image with all the NA values replaced by linearly interpolated values
#' in the y direction averaged with the linearly interpolated values in the x direction
#' @examples
#' completedIm <- completeImage(matrix(c(1,2,3,4), nrow = 2))
#'
#' @export
completeImage <- function(image) {
  numrows <- nrow(image)
  numcols <- ncol(image)
  image[1,] <- 0
  image[,1] <- 0
  image[numrows,] <- 0
  image[,numcols] <- 0
  imageRows <- image
  imageCols <- image
  for (ii in 1:numrows) {
    imageRows[ii,] <- completeSeq(image[ii,])
  }
  for (ii in 1:numcols) {
    imageCols[,ii] <- completeSeq(image[,ii])
  }
  return((imageRows + imageCols)/2)
}
