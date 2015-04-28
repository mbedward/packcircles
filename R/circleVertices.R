#' Generate vertex coordinates for a circle.
#' 
#' Generates vertex coordinates for a circle with given centre and radius.
#' 
#' @param xc centre X
#' @param yc centre Y
#' @param r radius
#' @param npoints number of vertices
#' 
#' @return A 2-column matrix of X and Y values.
#' 
#' @seealso \code{\link{circlePlotData}}
#' 
#' @export
#' 
circleVertices <- function(xc, yc, r, npoints=25) {
  a <- seq(0, 2*pi, length.out = npoints + 1)
  x <- xc + r * cos(a)
  y <- yc + r * sin(a)
  m <- cbind("x" = x, "y" = y)
}
