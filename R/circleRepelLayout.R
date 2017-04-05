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
#' @param x Either a vector of circle sizes (areas or radii) or a matrix or 
#'   data frame with a column of sizes and, optionally, columns for initial
#'   x-y coordinates of circle centres.
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
#' @param xysizecols The integer indices or names of the columns in \code{x} 
#'   for the centre x-y coordinates and sizes of circles. This argument is
#'   ignored if \code{x} is a vector.  If \code{x} is a matrix or data frame
#'   but does not contain initial x-y coordinates, this can be indicated as
#'   \code{xysizecols = c(NA, NA, 1)} for example.
#'   
#' @param sizetype The type of size values: either \code{"area"} or \code{"radius"}.
#'   May be abbreviated.
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
#'   data frame (centre x, centre y, radius).} \item{niter}{Number of iterations
#'   performed.} }
#'   
#' @export
#' 
circleRepelLayout <- function(x, xlim, ylim, 
                              xysizecols = c(1, 2, 3),
                              sizetype = c("area", "radius"),
                              maxiter=1000, wrap=TRUE, weights=1.0) {
  
  sizetype = match.arg(sizetype)
  
  xcol <- xysizecols[1]
  ycol <- xysizecols[2]
  sizecol <- xysizecols[3]
  
  if (missing(xlim)) xlim <- NULL
  xlim <- .checkBounds(xlim)
  
  if (missing(ylim)) ylim <- NULL
  ylim <- .checkBounds(ylim)

  if (is.matrix(x)) x <- as.data.frame(x)
  
  # get circle sizes and centre coordinates
  if (is.data.frame(x)) {
    sizes <- as.numeric(x[[sizecol]])
    
    if (is.na(xcol)) xcentres <- .initial_ordinates(length(sizes), xlim) 
    else xcentres <- as.numeric(x[[xcol]])
    
    if (is.na(ycol)) ycentres <- .initial_ordinates(length(sizes), ylim) 
    else ycentres <- as.numeric(x[[ycol]])
  }
  else {
    # x is a vector of circle sizes
    sizes <- as.numeric(x)
    xcentres <- .initial_ordinates(length(sizes), xlim)
    ycentres <- .initial_ordinates(length(sizes), ylim)
  }
  

  if (any(sizes <= 0, na.rm = TRUE)) {
    sizes[ sizes <= 0 ] <- NA
  }
  missing <- is.na(sizes) | is.nan(sizes)
  
  if (all(missing)) stop("all sizes are missing and/or non-positive")
  
  if (any(missing)) warning("missing and/or non-positive sizes will be ignored")
  
  
  # convert sizes from area to radii if required
  if (sizetype == "area") sizes <- sqrt(sizes / pi)
  
  
  # Matrix to pass to Rcpp
  xyr <- matrix( c(xcentres, ycentres, sizes), ncol = 3)
  colnames(xyr) <- c("x", "y", "radius")

  
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

  
  # Drop any missing data before passing to Rcpp
  xyr <- xyr[!missing, , drop=FALSE]
  weights <- weights[!missing]
  
  
  # Run Rcpp function which modifies xyr in place
  niter = iterate_layout(xyr, weights, xlim[1], xlim[2], ylim[1], ylim[2], maxiter, wrap)
  
  
  # Restore missing data if required
  if (any(missing)) {
    res <- xyr
    xyr <- matrix(NA_real_, nrow = length(sizes), ncol = 3)
    colnames(xyr) <- colnames(res)
    xyr[!missing, ] <- res
  }

  list(layout = as.data.frame(xyr), niter = niter)
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


#' @importFrom stats rnorm
#' 
.initial_ordinates <- function(n, limits) {
  if (anyNA(limits) || length(limits) < 2) 
    stop("Invalid limits: ", limits)
  
  infs <- is.infinite(limits)
  if (all(infs)) {
    limits <- c(0, 1)
  }
  else if (infs[1]) {
    limits[1] <- limits[2] - 1
  }
  else if (infs[2]) {
    limits[2] <- limits[1] + 1
  }
  
  o <- mean(limits)
  span <- limits[2] - limits[1]
  
  o + rnorm(n, 0, span/10)
}


#' Arranges circles by iterative pair-wise repulsion within a bounding rectangle
#' 
#' This function is deprecated and will be removed in a future release. 
#' Please use \code{\link{circleRepelLayout}} instead.
#' 
#' @note This function assumes that circle sizes are expressed as radii
#' whereas the default for \code{circleRepelLayout} is area. 
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
#' @return A list with components: 
#'   \describe{ 
#'    \item{layout}{A 3-column matrix or data.frame (centre x, centre y, radius).} 
#'    \item{niter}{Number of iterations performed.} 
#'   }
#'   
#' @seealso \code{\link{circleRepelLayout}}
#'   
#' @export
#' 
circleLayout <- function(xyr, xlim, ylim, 
                         maxiter=1000, wrap=TRUE, weights=1.0) {
  
  circleRepelLayout(xyr, xlim, ylim, 
                    xysizecols = 1:3,
                    sizetype = "radius",
                    maxiter, wrap, weights)
}
