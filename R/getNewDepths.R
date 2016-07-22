#' getNewDepths gives a 2d array of adjusted depth values
#'
#' @param image is the original 2d matrix with depth values representing the 3d image
#' @param newDom is a data.frame where the first column ($newCoordi) are the new i coords,
#' and the second column ($newCoordj) contains the new j coords
#' @param plane is data.frame containing a b and c where z = a + bx + cy
#' @param P is the 2d matrix containing shortest distances from each point to plane
#' @return a 2d matrix with the updated depth values
#'
#' @export
getNewDepths <- function(image, newDom, plane, P) {
  print("hello")
  a <- plane$a
  b <- plane$b
  c <- plane$c
  iMat <- newDom[[1]]
  jMat <- newDom[[2]]
  iMat <- iMat - min(iMat[!is.na(iMat)]) + 1
  jMat <- jMat - min(jMat[!is.na(jMat)]) + 1
  maxCoord <- max(max(iMat[!is.na(iMat)]), max(jMat[!is.na(jMat)]))
  size <- maxCoord
  rotatedBullet <- matrix(rep(NA,size^2), nrow = size)
  origSize <- nrow(image)
  for (ii in 1:origSize) {
    for (jj in 1:origSize) {
      if (!is.na(image[ii,jj])) {
        iCoord <- iMat[ii,jj]
        jCoord <- jMat[ii,jj]
        if (image[ii,jj] > a + b*ii + c*jj) {
          rotatedBullet[iCoord,jCoord] <- a + P[ii,jj]
        } else {
          rotatedBullet[iCoord,jCoord] <- a - P[ii,jj]
        }
      }
    }
  }
  return(rotatedBullet)
}


