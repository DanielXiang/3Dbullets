#' removeExtreme removes extreme parts of an image
#'
#' @param image is the original 2d array of depth values
#' @return the same image but with the extreme depth values removed
#' @examples
#' breechFace <- removeExtreme(matrix(c(1,2,3,4), nrow = 2))
#'
#' @export
removeExtreme <- function(image) {
  tol <- 25
  avgDepth <- mean(image[!is.na(image)])
  boolMat <- (image < avgDepth - tol) | (image > avgDepth + tol)
  image[boolMat] <- NA
  return(image)
}

