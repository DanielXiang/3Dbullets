#' getNewDomain gives a vector where the ith row has the new i and j coordinates
#'
#' @param image is a 2d matrix with depth values representing the 3d image
#' @param plane is data.frame containing a b and c where z = a + bx + cy
#' @return a data.frame with two columns, the first being a mapping from 1:size(image) to the
#' new i coordinate domain, the second being a mapping from 1:size(image) to the new j
#' coordinate domain
#' @return (ignore above return) a 2d matrix where the ijth entry is the new ij coordinate
#' @examples
#' newDom <- getNewDomain(matrix(c(1,2,3,4), nrow = 2),
#' data.frame(a = 141.8, b = .0584, c = -0.0146))
#'
#' @export
getNewDomain <- function(image, plane) {
  a <- plane$a
  b <- plane$b
  c <- plane$c
  size <- nrow(image)
  newDomi <- matrix(data = rep(NA,size^2), nrow = size, ncol = size)
  newDomj <- matrix(data = rep(NA,size^2), nrow = size, ncol = size)
  for (ii in 1:size) {
    for (jj in 1:size) {
      if (!is.na(image[ii,jj])) {
        t <- -(a + b*ii + c*jj - image[ii,jj])/(b^2+c^2+1)
        xi <- ii + t*b
        yi <- jj + t*c
        xStar <- (2*xi - 2*yi*(b/c)) / (2 + 2*b^2 / c^2)
        yStar <- -(b/c) * xStar
        Lmin <- sqrt((xi - xStar)^2 + (yi + b/c*xStar)^2 + (b*xi + c*yi)^2)
        alpha <- 1 + c^2/b^2
        beta <- (-1)*(2*xStar + (2*c/b)*(b*xStar/c + c*xStar/b) + 2*yStar*c/b)
        gamma <- xStar^2 + (b*xStar/c + c*xStar/b)^2 +
          2*yStar*(b*xStar/c + c*xStar/b) + yStar^2 - Lmin^2
        if (yi < -b*xi/c) {
          iiNew <- (-beta - sqrt(beta^2 - 4*alpha*gamma))/(2*alpha)
          jjNew <- c*iiNew/b - (b*xStar/c + c*xStar/b)
          iiNew <- round(iiNew)
          jjNew <- round(jjNew)
          newDomi[ii,jj] <- iiNew
          newDomj[ii,jj] <- jjNew
        } else {
          iiNew <- (-beta + sqrt(beta^2 - 4*alpha*gamma))/(2*alpha)
          jjNew <- c*iiNew/b - (b*xStar/c + c*xStar/b)
          iiNew <- round(iiNew)
          jjNew <- round(jjNew)
          newDomi[ii,jj] <- iiNew
          newDomj[ii,jj] <- jjNew
        }
      }
    }
  }
  out <- vector(mode="list", length = 2)
  out[[1]] <- newDomi
  out[[2]] <- newDomj
  return(out)
}
