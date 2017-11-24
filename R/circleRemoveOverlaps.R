#' Filters a set of circles to remove all overlaps
#' 
#' Given an initial set of circles, this function identifies a subset of
#' non-overlapping circles using a simple heuristic algorithm. Circle positions
#' remain fixed.
#' 
#' The \code{method} argument specifies whether to use the heuristic algorithm or
#' linear programming. The following options select the heuristic algorithm and
#' specify how to choose an overlapping circle for rejection at each iteration: 
#'   \describe{
#'     \item{maxov}{Choose one of the circles with the greatest number of overlaps.}
#'     \item{minov}{Choose one of the circle with the least number of overlaps.}
#'     \item{largest}{Choose one of the largest circles.}
#'     \item{smallest}{Choose one of the smallest circles.}
#'     \item{random}{Choose a circle at random.}
#'   }
#' At each iteration the number of overlaps is checked for each candidate
#' circle and any non-overlapping circles added to the selected subset. Then a
#' single overlapping circle is chosen, based on the method being used, from
#' among the remainder and marked as rejected. Iterations continue until all
#' circles have been either selected or rejected. The 'maxov' option (default)
#' generally seems to perform best at maximizing the number of circles retained.
#' The other options are provided for comparison and experiment. Beware that
#' some can perform surprisingly poorly, especially 'minov'.
#'   
#' Two further options select linear programming:
#'   \describe{
#'     \item{lparea}{Maximise the total area of circles in the subset.}
#'     \item{lpnum}{Maximise the total number of circles in the subset.}
#'   }
#'   
#' The `lpSolve` package must be installed to use the linear programming options.
#' These options will find an optimal subset, but for anything other than a small
#' number of initial circles the running time can be prohibitive.
#' 
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
  
  if (method == "lparea") {
    # maximise radius squared
    f.obj <- xyr[, "radius"]^2
  }
  else {
    # for lpnum
    f.obj <- rep(1, nrow(xyr))
  }
  
  terms <- .lp_make_terms(xyr)

  res <- lpSolve::lp("max", f.obj, 
                     const.dir = terms$f.dir,
                     const.rhs = terms$f.rhs,
                     dense.const = terms$f.con,
                     scale = 0,
                     all.bin = TRUE)
  
  # return selections as logical vector
  res$solution > 0
}


.lp_make_terms <- function(xyr) {
  N <- nrow(xyr)
  d <- as.matrix( stats::dist(xyr[, c("x", "y")]) )
  
  r <- xyr[, "radius"]
  rsums <- outer(r, r, "+")
  
  overlaps <- 1 * (d < rsums)
  numovs <- rowSums(overlaps)
  
  overlaps[ lower.tri(overlaps) ] <- 0
  rs <- rowSums(overlaps)
  cons <- lapply(1:N, function(i) {
    if (rs[i] > 1) {
      ids <- which(overlaps[i, ] == 1)
      n <- length(ids) - 1
      x <- matrix(1, nrow=2*n, ncol=2)
      oddrows <- 1:nrow(x) %% 2 == 1
      x[oddrows, 1] <- i
      x[!oddrows, 1] <- ids[-1]
      x
    }
  })
  
  cons <- do.call(rbind, cons)
  cons <- cbind(rep(1:(nrow(cons)/2), each=2), cons)
  colnames(cons) <- c("constr.id", "circle", "value")
  
  f.dir <- rep("<=", nrow(cons)/2)

  # non-overlapping circles
  ids <- which(numovs == 1)
  n <- length(ids)
  
  if (n > 0) {
    id0 <- max(cons[, "constr.id"])
    
    cons <- rbind(
      cons,
      
      cbind(constr.id = (id0+1):(id0 + n),
            circle = ids,
            value = 1) )
    
    f.dir <- c(f.dir, rep("==", n))
  }
  
  list(f.con = cons, f.dir = f.dir, f.rhs = rep(1, cons[nrow(cons), "constr.id"]))
}
