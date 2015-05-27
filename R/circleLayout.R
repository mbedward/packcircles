#' Find non-overlapping layout of circles.
#' 
#' Attempts to find a layout for a given set of circles
#' within a rectangle such that is no overlap between circles.
#' 
#' @param xyr 3-column matrix or data.frame (centre X, centre Y, radius)
#' @param xlim bounds in the X direction; either a vector for [xmin, xmax)
#'   or a single value interpreted as [0, xmax)
#' @param ylim bounds in the Y direction; either a vector for [ymin, ymax)
#'   or a single value interpreted as [0, ymax)
#' @param maxiter maximum number of iterations to attempt
#' @param wrap whether to allow coordinate wrapping across bounds. If `true`,
#'   coordinate wrapping results in a toroidal space; if `false`, ordinates
#'   are simply restricted to bounds.
#' 
#' @return A list with components:
#'   \describe{
#'     \item{layout}{A 3-column matrix or data.frame (centre x, centre y, radius).}
#'     \item{niter}{Number of iterations performed.}
#'   }
#' 
#' @export
#' 
circleLayout <- function(xyr, xlim, ylim, maxiter=1000, wrap=TRUE) {
  if ( !(is.data.frame(xyr) || is.matrix(xyr)) )
    stop("argument xyr must be a data.frame or matrix")
  
  if (ncol(xyr) != 3)
    stop("argument xyr must have 3 columns (x, y, radius)")
  
  if (is.data.frame(xyr))
    m <- as.matrix(xyr)
  else {
    # clone the matrix to prevent the input object 
    # being altered
    m <- matrix(as.numeric(xyr), nrow=nrow(xyr), ncol=ncol(xyr))
    colnames(m) <- colnames(xyr)
  }

  xlim <- .checkBounds(xlim)
  ylim <- .checkBounds(ylim)
  
  niter = iterate_layout(m, xlim[1], xlim[2], ylim[1], ylim[2], maxiter, wrap)

  if (is.data.frame(xyr)) m <- as.data.frame(m)
  list(layout = m, niter = niter)
}


.checkBounds <- function(bounds) {
  len <- length(bounds)
  arg.name <- deparse(substitute(bounds))

  if (len == 1) {
    bounds <- c(0, bounds)
  } else if (len == 2) {
    bounds <- c( min(bounds), max(bounds) )
  } else {
    stop(arg.name, " has length ", len, "; expected 1 or 2")
  }
  
  if (bounds[1] == bounds[2]) {
    stop(arg.name, " min and max should not be equal (", bounds, ")")
  }
  
  bounds
}
