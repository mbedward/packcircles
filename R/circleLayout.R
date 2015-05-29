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
#' @param weights an optional vector of numeric weights (0 to 1 inclusive) 
#'   to apply to the distance each circle moves during pair-repulsion. 
#'   A weight of 0 prevents any movement. A weight of 1 gives the default
#'   movement distance. A single value can be supplied for uniform weights.
#'   A vector with length less than the number of circles will be silently 
#'   extended by repeating the final value. Any values outside the range
#'   [0, 1] will be clamped to 0 or 1.
#' 
#' @return A list with components:
#'   \describe{
#'     \item{layout}{A 3-column matrix or data.frame (centre x, centre y, radius).}
#'     \item{niter}{Number of iterations performed.}
#'   }
#' 
#' @export
#' 
circleLayout <- function(xyr, xlim, ylim, maxiter=1000, wrap=TRUE, weights=1.0) {
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
  
  if (is.null(weights) || length(weights) == 0) 
    weights <- rep(1.0, nrow(xyr))
  else {
    if (!is.numeric(weights))
      stop("weights must be a numeric vector with values between 0 and 1")
    
    if (length(weights) < nrow(xyr)) {
      i <- length(weights)
      weights <- c(weights, rep(weights[i], nrow(xyr) - i))
    } else if (weights > nrow(xyr)) {
      weights <- weights[1:nrow(xyr)]
    }
    
    # clamp values to be in the range [0, 1]
    weights[ weights < 0 ] <- 0
    weights[ weights > 1 ] <- 1
  }

  xlim <- .checkBounds(xlim)
  ylim <- .checkBounds(ylim)
  
  niter = iterate_layout(m, weights, xlim[1], xlim[2], ylim[1], ylim[2], maxiter, wrap)

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
