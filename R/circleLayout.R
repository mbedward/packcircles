#' Find non-overlapping layout of circles.
#' 
#' Attempts to find a layout for a given set of circles
#' within a rectangle such that is no overlap between circles.
#' 
#' @param xyr 3-column matrix or data.frame (centre X, centre Y, radius)
#' @param xbound X upper limit (lower assumed 0)
#' @param ybound Y upper limit (lower assumed 0)
#' @param max.iter maximum number of iterations to attempt
#' 
#' @return A list with components:
#'   \describe{
#'     \item{layout}{A 3-column matrix or data.frame (centre x, centre y, radius).}
#'     \item{niter}{Number of iterations performed.}
#'   }
#' 
#' @export
#' 
circleLayout <- function(xyr, xbound, ybound, max.iter=1000) {
  if ( !(is.data.frame(xyr) || is.matrix(xyr)) )
    stop("argument xyr must be a data.frame or matrix")
  
  if (ncol(xyr) != 3)
    stop("argument xyr must have 3 columns (x, y, radius)")
  
  m <- as.matrix(xyr)
  niter = iterate_layout(m, xbound, ybound, max.iter)

  if (is.data.frame(xyr)) m <- as.data.frame(m)
  list(layout = m, niter = niter)
}
