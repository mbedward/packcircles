#' Arranges circles by iterative pair-wise repulsion within a bounding rectangle
#' 
#' This function takes a set of circles, defined by a data frame of initial 
#' centre positions and radii, and uses iterative pair-wise repulsion to try to 
#' find a non-overlapping arrangement where all circle centres lie inside a 
#' bounding rectangle. If no such arrangement can be found within the specified 
#' maximum number of iterations, the last attempt is returned.
#' 
#' The algorithm is adapted from a demo written in the Processing language by 
#' \href{http://www.cricketschirping.com/processing/CirclePacking1/CirclePacking1.pde}{Sean McCullough}.
#' Each circle in the input data is compared to those following it. If two 
#' circles overlap, they are moved apart such that the distance moved by each is
#' proportional to the radius of the other, loosely simulating inertia. So when 
#' a small circle is overlapped by a larger circle, the small circle moves 
#' furthest. This process is repeated until no more movement takes place 
#' (acceptable layout) or the maximum number of iterations is reached (layout 
#' failure).
#' 
#' To avoid edge effects, the bounding rectangle can be treated as a toroid by 
#' setting the \code{wrap} argument to \code{TRUE}. With this option, a circle 
#' moving outside the bounds re-enters at the opposite side.
#' 
#' 
#' @param xyr A 3-column matrix or data frame (centre X, centre Y, radius).
#'   
#' @param xlim The bounds in the X direction; either a vector for [xmin, xmax) 
#'   or a single value interpreted as [0, xmax). Alternatively, omitting this 
#'   argument or passing any of \code{NULL}, a vector of \code{NA} or an empty
#'   vector will result in unbounded movement in the X direction.
#'   
#' @param ylim The bounds in the Y direction; either a vector for [ymin, ymax) 
#'   or a single value interpreted as [0, ymax). Alternatively, omitting this 
#'   argument or passing any of \code{NULL}, a vector of \code{NA} or an empty
#'   vector will result in unbounded movement in the Y direction.
#'   
#' @param maxiter The maximum number of iterations.
#'   
#' @param wrap Whether to treat the bounding rectangle as a toroid (default 
#'   \code{TRUE}). When this is in effect, a circle leaving the bounds on one 
#'   side re-enters on the opposite side.
#'   
#' @param weights An optional vector of numeric weights (0 to 1 inclusive) to 
#'   apply to the distance each circle moves during pair-repulsion. A weight of 
#'   0 prevents any movement. A weight of 1 gives the default movement distance.
#'   A single value can be supplied for uniform weights. A vector with length 
#'   less than the number of circles will be silently extended by repeating the 
#'   final value. Any values outside the range [0, 1] will be clamped to 0 or 1.
#'   
#' @return A list with components: \describe{ \item{layout}{A 3-column matrix or
#'   data.frame (centre x, centre y, radius).} \item{niter}{Number of iterations
#'   performed.} }
#'   
#' @export
#' 
circleRepelLayout <- function(xyr, xlim, ylim, maxiter=1000, wrap=TRUE, weights=1.0) {
  
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
    } else if (length(weights) > nrow(xyr)) {
      weights <- weights[1:nrow(xyr)]
    }
    
    # clamp values to be in the range [0, 1]
    weights[ weights < 0 ] <- 0
    weights[ weights > 1 ] <- 1
  }

  if (missing(xlim)) xlim <- NULL
  xlim <- .checkBounds(xlim)
  
  if (missing(ylim)) ylim <- NULL
  ylim <- .checkBounds(ylim)
  
  niter = iterate_layout(m, weights, xlim[1], xlim[2], ylim[1], ylim[2], maxiter, wrap)

  if (is.data.frame(xyr)) m <- as.data.frame(m)
  list(layout = m, niter = niter)
}


.checkBounds <- function(bounds) {
  if (is.null(bounds[1]) || is.na(bounds[1]) || length(bounds) == 0) {
    bounds <- c(-Inf, Inf)
  }
  else {
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
  }
  
  bounds
}


#' Arranges circles by iterative pair-wise repulsion within a bounding rectangle
#' 
#' This is the deprecated name for \code{\link{circleRepelLayout}} and will be
#' removed in a future package update.
#' 
#' @inherit circleRepelLayout
#'   
#' @export
#' 
circleLayout <- function(xyr, xlim, ylim, maxiter=1000, wrap=TRUE, weights=1.0) {
  circleRepelLayout(xyr, xlim, ylim, maxiter, wrap, weights)
}
