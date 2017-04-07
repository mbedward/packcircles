#' Filters a set of circles to remove all overlaps
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
#' @return A data frame with centre coordinates and radii of selected circles.
#'   
#' @export
#' 
circleRemoveOverlaps <- function(x, 
                                 xysizecols = 1:3, 
                                 sizetype = c("area", "radius"),
                                 tolerance = 1.0) {

    sizetype = match.arg(sizetype)
    
    if (tolerance <= 0) stop("tolerance must be positive (default is 1.0)")
    
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
    
    
    # Matrix to pass to Rcpp
    xyr <- matrix( c(xcentres, ycentres, sizes), ncol = 3)
    colnames(xyr) <- c("x", "y", "radius")
    
    # Drop any missing data before passing to Rcpp
    xyr <- xyr[!missing, , drop=FALSE]
    
    # Run Rcpp function and get logical vector defining
    # selected subset of circles
    selected <- select_non_overlapping(xyr, tolerance);
    
    # Return data frame of selected circles
    as.data.frame(xyr[selected, , drop = FALSE])
}

