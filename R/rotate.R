#' rotate runs the entire rotation process
#'
#' @param x3pFile is the original list vector of data whose first element is the
#' metadata, and the second element is the 2D array of depth values.
#' @return the same file, but the 2D array of depth values is now leveled out.
#'
#' @export
rotate <- function(x3pFile) {
  image <- x3pFile[[2]]
  small_size <- min(nrow(image), ncol(image))
  big_size <- max(nrow(image), ncol(image))
  if (small_size == nrow(image)) {
    diff <- big_size - small_size
    margin <- ceiling(diff/2)
    image <- image[, margin:(margin+small_size-1)]
  } else {
    diff <- big_size - small_size
    margin <- ceiling(diff/2)
    image <- image[margin:(margin+small_size-1),]
  }
  breechFace <- rmFPradius(image)
  top <- localRemove(breechFace)
  plane <- getPlane(top)
  distances <- getShortestDist(image, plane)
  newDom <- getNewDomain(image, plane)
  rotatedBullet <- getNewDepths(image, newDom, plane, distances)
  # comment the above three lines and uncomment the below line if the slant is small
  #rotatedBullet <- getNewDepthsApprox(image, plane)
  x3pFile[[2]] <- rotatedBullet
  minsize <- min(nrow(rotatedBullet), ncol(rotatedBullet))
  x3pFile[[1]]["num.lines"] <- minsize
  x3pFile[[1]]["num.pts.line"] <- minsize
  return(x3pFile)
}


