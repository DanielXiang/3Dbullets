#' getPlane gets the fitted coefficients for the entire image
#'
#' Do a multiple regression on the depth values of the basis functions and
#' return the coefficients a, b, and c
#'
#' @param image is a 2d matrix with depth values representing the 3d image
#' @return A data frame with 3 elements. The "a", "b", and "c" components are the parameters of the
#' plane which takes the form z = a + bx + cy
#' @examples
#' coeffs <- getPlane(matrix(c(1,2,3,4), nrow = 2))
#'
#' @export
getPlane <- function(image) {
  size <- sum(!is.na(image))
  n <- nrow(image)
  iCoords <- c(1:size)
  jCoords <- c(1:size)
  depths <- c(1:size)
  counter <- 0
  for (ii in 1:n) {
    for (jj in 1:n) {
      if (!is.na(image[ii,jj])) {
        counter = counter + 1
        iCoords[counter] <- ii
        jCoords[counter] <- jj
        depths[counter] <- image[ii,jj]
      }
    }
  }
  regression <-
    lm(depths ~ iCoords + jCoords, data = data.frame(z = depths, x = iCoords, y = jCoords))
  regCoef <- regression$coefficients
  return(data.frame(a = regCoef[1], b = regCoef[2], c = regCoef[3]))
}

