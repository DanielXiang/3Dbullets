#' rmFPradius removes the firing pin from the 3d image by finding the center coordinates,
#' and then removing all pixels within a specified radius of that center.
#'
#' @param image is the original 2d array of depth values
#' @param rad is the radius of the circle centered at the firing pin center
#' you want to filter out
#' @return the same image but with the firing pin removed
#' @examples
#' breechFace <- rmFPradius(matrix(c(1,2,3,4), nrow = 2))
#'
#' @export
rmFPradius <- function(image, rad = 145000) {
  center <- findFPCenter(image)
  iCent <- center$i
  jCent <- center$j
  size <- nrow(image)
  newImage <- image
  for (ii in 1:size) {
    for (jj in 1:size) {
      if ((ii-iCent)^2 + (jj - jCent)^2 < rad) {
        newImage[ii,jj] <- NA
      }
    }
  }
  return(newImage)
}

