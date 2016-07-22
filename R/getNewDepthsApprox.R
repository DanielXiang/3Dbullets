#' getNewDepthsApprox gives a 2d array of adjusted depth values where the new best
#' fit plane is a level plane
#'
#' @param image is the original 2d matrix with depth values representing the 3d image
#' @param plane is data.frame containing a b and c where z = a + bx + cy
#' @return a 2d matrix with the updated depth values
#' @examples
#' getNewDepthsApprox <-
#' getNewDepthsApprox(matrix(c(1,2,3,4), nrow = 2),
#' data.frame(a = 1, b = 2, c = 3))
#'
#' @export
getNewDepthsApprox <- function(image, plane) {
  a <- plane$a
  b <- plane$b
  c <- plane$c
  size <- nrow(image)
  halfSize <- size/2
  iValues <- matrix(rep(c((-halfSize):(halfSize-1)),size), ncol = size, nrow = size)
  jValues <- matrix(rep(NA,size^2), nrow = size, ncol = size)
  for (jj in 0:size-1) {
    jValues[,jj] <- rep(jj - halfSize, size)
  }
  size <- nrow(image)
  rotatedBullet <- image
  rotatedBullet = image - b*iValues - c*jValues
  return(rotatedBullet)
}


