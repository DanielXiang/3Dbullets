#' rmFPavg removes the firing pin from the 3d image by finding the average pixel depth,
#' and then removing all pixels with a depth greater than .853 * (avg pixel depth)
#'
#' @param image is the original 2d array of depth values
#' @return the same image but with the firing pin removed
#' @examples
#' breechFace <- rmFPavg(matrix(c(1,2,3,4), nrow = 2))
#'
#' @export
rmFPavg <- function(image) {
  size <- nrow(image)
  nonNAimage <- image[!is.na(image)]
  avgDepth <- mean(nonNAimage)
  indices <- image > avgDepth*.853
  image[indices] <- NA
  return(image)
}

