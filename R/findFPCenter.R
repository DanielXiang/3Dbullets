#' findFPCenter finds the i j coordinates of the center of the firing pin
#'
#' @param image is the original 2d matrix with depth values representing the 3d image
#' @return an (i,j) coordinate representing the center of the firing pin
#' @examples
#' center <- findFPCenter(matrix(c(1,2,3,4), nrow = 2))
#'
#' @export
findFPCenter <- function(image) {
  size <- nrow(image)
  nonNAimage <- image[!is.na(image)]
  avgDepth <- mean(nonNAimage)
  indices <- (image > avgDepth) & (!is.na(image))
  tallCoords <- cbind(c(NA),c(NA))
  for (ii in 1:size) {
    truesLen <- length(which(indices[ii,]))
    iCoords <- rep(ii,truesLen)
    jCoords <- which(indices[ii,])
    tallCoords <- rbind(tallCoords, cbind(iCoords,jCoords))
  }
  tallCoords <- tallCoords[-1,]
  return(data.frame(i = round(mean(tallCoords[,1])), j = round(mean(tallCoords[,2]))))
}


