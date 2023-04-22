#' @title Make Factor
#' @description It converts a list of ROIs from a single into factors, see Cardinal:makeFactor()
#' @return a vector
#' @noRd
#' @examples
#' a = c(FALSE, FALSE, TRUE, FALSE, FALSE)
#' b = c(TRUE, TRUE, FALSE, FALSE, FALSE)
#' c = c(FALSE, TRUE, TRUE, FALSE, FALSE)
#' d = c(FALSE, FALSE, TRUE, FALSE, FALSE)
#' e = c(TRUE, TRUE, FALSE, FALSE, FALSE)
#' f = c(TRUE, TRUE, FALSE, FALSE, FALSE)
#' l = list(a, b, c, d, e, f)
#' names(l) = c("a:X", "b:X", "c:Y", "d:Y", "e:Z", "f:Z") # here 6 ROIs from single run X
#' makeFactor2(l)

makeFactor2 <- function(roiList, ordered = FALSE) {
  labs <- names(roiList)
  names(labs) <- NULL
  roiList <- do.call("cbind", roiList)
  roiList <- apply(roiList, 1, function(i) which(i)[1L])
  factor(labs[roiList], levels = labs, ordered = ordered)
}

#' @title Get Factor
#' @description It converts a list of ROIs from different runs into a list of factors
#' @return a vector
#' @noRd
#' @examples
#' a = c(FALSE, FALSE, TRUE, FALSE, FALSE)
#' b = c(TRUE, TRUE, FALSE, FALSE, FALSE)
#' c = c(FALSE, TRUE, TRUE, FALSE, FALSE)
#' d = c(FALSE, FALSE, TRUE, FALSE, FALSE)
#' e = c(TRUE, TRUE, FALSE, FALSE, FALSE)
#' f = c(TRUE, TRUE, FALSE, FALSE, FALSE)
#' l = list(a, b, c, d, e, f)
#' names(l) = c("a:X", "b:X", "c:Y", "d:Y", "e:Z", "f:Z") # here 6 ROIs from 3 runs X,Y,Z
#' getFactor(l)

getFactor <- function(roiList, ...) {
  name_parts <- factor(sapply(strsplit(names(roiList), split = ":"), "[", 2))
  split_list <- split(roiList, name_parts)
  lapply(split_list, makeFactor2)
}


