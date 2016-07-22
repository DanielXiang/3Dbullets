#' bilrot takes an image and rotates it by theta using bilinear interpolation to fill
#' in the gaps.
#'
#' @param theta is the angle by which you want to rotate (in degrees)
#' @param image is the 2D array of depth values
#' @return a 2d matrix with the updated depth values
#' @examples
#' rotImage <- bilrot(45, matrix(c(1,2,3,4), nrow = 2))
#'
#' @export
bilrot <- function(theta, image) {
  completed <- completeImage(image)
  # convert to radians
  rotation <- theta * pi / 180
  rotationMat <- matrix(c(cos(rotation), sin(rotation), -sin(rotation), cos(rotation)), nrow = 2)
  numrows <- nrow(image)
  numcols <- ncol(image)
  newImage <- image
  center <- matrix(c(round(numcols/2), round(numrows/2)), nrow = 2)
  # perform rotation and bilinear interpolation
  for (jj in 1:numcols) {
    for (ii in 1:numrows) {
      oldCoords <- matrix(c(jj,ii), nrow = 2) - center
      newCoords <- rotationMat %*% oldCoords
      newJ <- newCoords[1] + center[1]
      newI <- newCoords[2] + center[2]
      X <- newJ
      Y <- newI
      ceilX <- ceiling(newJ)
      floorX <- floor(newJ)
      ceilY <- ceiling(newI)
      floorY <- floor(newI)
      if ((ceilX > numcols) | (ceilY > numrows) | (floorX < 1) | (floorY < 1)) {
        newDepth <- 0
      } else {
        xHolder1 <- (ceilX - X)*completed[floorY, ceilX] + (X - floorX)*completed[floorY, floorX]
        xHolder2 <- (ceilX - X)*completed[ceilY, ceilX] + (X - floorX)*completed[ceilY, floorX]
        newDepth <- (Y - floorY)*xHolder1 + (ceilY - Y)*xHolder2
      }
      newImage[ii,jj] <- newDepth
    }
  }
  return(newImage)
}

