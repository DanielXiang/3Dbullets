#' completeSeq takes a vector of numbers that may contain NA values and recursively
#' fills in the NA values by interpolating the about a symmetric string of NA's. This
#' method assumes the first and last element of the vector to be non NA values.
#'
#' @param row is the vector whose first and last elements are non NA values
#' @return row but all the NA's are linearly interpolated
#' @examples
#' completedVec <- completeSeq(c(1,NA,NA,3,NA,NA,NA,9))
#'
#' @export
completeSeq <- function(row) {
  nonNA <- which(!is.na(row))
  if (length(nonNA) == length(row)) {
    return(row)
  } else {
    for (ii in 1:(length(nonNA)-1)) {
      candidate <- ceiling((nonNA[ii] + nonNA[ii + 1])/2)
      if (candidate != (nonNA[ii + 1])) {
        row[candidate] <- (row[nonNA[ii]] + row[nonNA[ii + 1]])/2
      }
    }
    return(completeSeq(row))
  }
}

