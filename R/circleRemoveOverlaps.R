#' Filters a set of circles to remove all overlaps
#' 
#' Given an initial set of circles, this function identifies a subset of
#' non-overlapping circles using a simple heuristic algorithm. At each
#' iteration the numnber of overlaps is checked for each candidate circle and
#' any non-overlapping circles added to the selected subset. Then a single 
#' overlapping circle is chosen (see Details) from among the remainder and marked
#' as rejected. Iterations continue until all circles have been either 
#' selected or rejected.
#' 
#' The \code{method} argument specifies whether to use variations of
#' the heuristic selection algorithm or linear programming (with package \code{lpSolve}):
#'   \describe{
#'     \item{maxov}{Choose a circle whose number of overlaps equals the current maximum.}
#'     \item{minov}{Choose a circle whose number of overlaps equals the current 
#'     (non-zero) minimum.}
#'     \item{largest}{Choose a circle whose size equals the current maximum size of 
#'     overlapping circles.}
#'     \item{smallest}{Choose a circle whose size equals the current minimum size of 
#'     overlapping circles.}
#'     \item{random}{Choose a circle at random.}
#'     \item{lp}{Use linear programming to find a solution maximising the total area
#'     of circles in the subset. Package lpSolve must be installed for this option.}
#'   }
#'   
#' When using the heuristic algorithm with a method other than \code{random}, overlapping
#' circles are assessed at each step according to the ordering criterion (e.g. having the
#' most number of overlaps) and then, if more than one circle meets the criterion, a
#' random selection is made from among them.
#' 
#' In tests with random configurations of uniform and non-uniform sized circles, the 
#' default \code{maxov} strategy seems to produce the densest subset of non-overlapping
#' circles while the \code{minov} strategy often produces very sparse subsets. However,
#' results will be highly data dependent.
#' 
#' @param x A matrix or data frame containing circle x-y centre coordinates
#' and sizes (area or radius).
#'   
#' @param xysizecols The integer indices or names of the columns in \code{x} for
#'   the centre x-y coordinates and sizes of circles. Default is \code{c(1,2,3)}.
#'   
#' @param sizetype The type of size values: either \code{"area"} (default) or 
#'   \code{"radius"}. May be abbreviated.
#'   
#' @param tolerance Controls the amount of overlap allowed. Set to 1 for 
#'   simple exclusion of overlaps. Values lower than 1 allow more overlap.
#'   Values > 1 have the effect of expanding the influence of circles so that
#'   more space is required between them. The input value must be > 0.
#'   
#' @param method Specifies whether to use linear programming (default) or one of
#'   the variants of the heuristic algorithm. Alternatives are:
#'   \code{"maxov"}, \code{"minov"}, \code{"largest"}, \code{"smallest"},
#'   \code{"random"}, \code{"lparea"}, \code{"lpnum"}. See Details for further
#'   explanation.
#'   
#' @return A data frame with centre coordinates and radii of selected circles.
#' 
#' @note \emph{This function is experimental} and will almost certainly change before
#' the next package release. In particular, it will probably return something
#' other than a data frame.
#'   
#' @export
#' 
circleRemoveOverlaps <- function(x, 
                                 xysizecols = 1:3, 
                                 sizetype = c("area", "radius"),
                                 tolerance = 1.0,
                                 method = c("maxov", "minov", 
                                            "largest", "smallest", "random",
                                            "lparea", "lpnum")) {

    sizetype = match.arg(sizetype)
    method = match.arg(method)
    
    # If one of the linear programming options has been specified
    # check that package lpSolve is installed.
    #
    using.lp <- method %in% c("lparea", "lpnum")
    if (using.lp) {
      if (!requireNamespace("lpSolve", quietly = TRUE)) {
        stop("Package lpSolve must be installed to use method='", method, "'", 
             call. = FALSE)
      }    
    }
    
    # If using heuristic algorithm, check tolerance argument
    if (!using.lp) {
      if (tolerance <= 0) stop("tolerance must be positive (default is 1.0)")
    }
    
    xcol <- xysizecols[1]
    ycol <- xysizecols[2]
    sizecol <- xysizecols[3]

    if (is.matrix(x)) x <- as.data.frame(x)
    
    # get circle sizes and centre coordinates
    sizes <- as.numeric(x[[sizecol]])
    xcentres <- as.numeric(x[[xcol]])
    ycentres <- as.numeric(x[[ycol]])
    
    if (any(sizes <= 0, na.rm = TRUE)) {
      sizes[ sizes <= 0 ] <- NA
    }
    missing <- is.na(sizes) | is.nan(sizes)
    
    if (all(missing)) stop("all sizes are missing and/or non-positive")
    
    if (any(missing)) warning("missing and/or non-positive sizes will be ignored")
    
    
    # convert sizes from area to radii if required
    if (sizetype == "area") sizes <- sqrt(sizes / pi)

    xyr <- matrix( c(xcentres, ycentres, sizes), ncol = 3)
    colnames(xyr) <- c("x", "y", "radius")
    
    # Drop any missing data before passing to Rcpp
    xyr <- xyr[!missing, , drop=FALSE]

    if (using.lp) {
      # Linear programming
      selected <- .lp_non_overlapping(xyr, method)
    } else {
      # Heuristic
      selected <- select_non_overlapping(xyr, tolerance, method);
    }
        
    
    # Return data frame of selected circles
    as.data.frame(xyr[selected, , drop = FALSE])
}


.lp_non_overlapping <- function(xyr, method = c("lparea", "lpnum")) {
  method = match.arg(method)
  
  if (method == "lparea") f.obj <- xyr[, "radius"]^2
  else f.obj <- rep(1, nrow(xyr))
  
  f.con <- .lp_make_constraint_matrix(xyr)
  f.dir <- rep("<=", nrow(f.con))
  f.rhs <- rep(1, nrow(f.con))
  
  res <- lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)
  
  # return selections as logical vector
  res$solution > 0
}


.lp_make_constraint_matrix <- function(xyr) {
  N <- nrow(xyr)
  d <- stats::dist(xyr[, c("x", "y")])
  
  m <- matrix(0, nrow=N, ncol=N)
  diag(m) <- 1
  
  k <- 1
  for (i in 1:(N-1)) {
    ir <- xyr[i, 3]
    for (j in (i+1):N) {
      if (d[k] < (ir + xyr[j, 3])) m[i, j] <- 1
      k <- k + 1
    }
  }  
  
  m
}
