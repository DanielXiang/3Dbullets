#' getShortestDist gives a 2d matrix where the ij entry is the shortest distance
#' from that point and depth to the plane given by the parameters a b and c
#'
#' @param image is a 2d matrix with depth values representing the 3d image
#' @param plane is data.frame containing a b and c where z = a + bx + cy
#' @return a 2d matrix containing the shortest distances
#' @examples
#' dists <- getShortestDist(matrix(c(1,2,3,4), nrow = 2),
#' data.frame(a = 141.8, b = .0584, c = -0.0146))
#'
#' @export
getShortestDist <- function(image, plane) {
  a <- plane$a
  b <- plane$b
  c <- plane$c
  size <- nrow(image)
  P <- matrix(rep(NA,size^2), nrow = size)
  for (ii in 1:size) {
    for (jj in 1:size) {
      if (!is.na(image[ii,jj])) {
        t <- (-1)*(a + b*ii + c*jj - image[ii,jj])/(b^2 + c^2 + 1)
        P[ii,jj] <- sqrt((t*b)^2 + (t*c)^2 + t^2)
      }
    }
  }
  return(P)
}
