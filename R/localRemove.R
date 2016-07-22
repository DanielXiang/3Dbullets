#' localRemove removes the extreme depth values from the image
#'
#' @param image is the original 2d array of depth values
#' @param windowSize is the size of the local region you want to filter
#' extreme values from
#' @return the same image but with the extreme depth values removed in
#' local regions whose sizes are determined by windowSize
#' @examples
#' breechFace <- localRemove(matrix(c(1,2,3,4), nrow = 2))
#'
#' @export
localRemove <- function(image, windowSize = 120) {
  imgSize <- nrow(image)
  div <- ceiling(imgSize/windowSize)
  for (ii in 1:div) {
    for (jj in 1:div) {
      topPrev <- (ii-1)*windowSize
      topNext <- min(ii*windowSize, imgSize)
      sidePrev <- (jj-1)*windowSize
      sideNext <- min(jj*windowSize, imgSize)
      region <- image[topPrev:topNext, sidePrev:sideNext]
      image[topPrev:topNext, sidePrev:sideNext] = removeExtreme(region)
    }
  }
  return(image)
}


