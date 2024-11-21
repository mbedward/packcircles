#' Generate a set of circle vertices suitable for plotting
#' 
#' Given a matrix or data frame for a circle layout, with columns for centre X
#' and Y coordinates and circle sizes, this function generates a data set of
#' vertices which can then be used with ggplot or base graphics functions. If
#' any of the size values in the input data are zero, negative or missing
#' (\code{NA} or \code{NULL}), the corresponding circles will not be generated.
#' This can be useful when displaying alternative subsets of circles.
#' 
#' @note \strong{Input sizes are assumed to be radii}. This is slightly
#'   confusing because the layout functions \code{circleRepelLayout} and
#'   \code{circleProgressiveLayout} treat their input sizes as areas by default.
#'   To be safe, you can always set the \code{sizetype} argument explicitly for
#'   both this function and layout functions.
#'   
#' @param layout A matrix or data frame of circle data (x, y, size). May also 
#'   contain other columns including an optional identifier column.
#'   
#' @param npoints The number of vertices to generate for each circle. 
#'   
#' @param xysizecols The integer indices or names of columns for the centre X, 
#'   centre Y and size values. Default is `c(1,2,3)`.
#'   
#' @param sizetype The type of size values: either \code{"radius"} (default) or 
#'   \code{"area"}. May be abbreviated.
#'   
#' @param idcol Optional index (integer) or name (character) of an input data
#'   column to use as circle identifier values in the \code{id} column of the
#'   output data frame. Identifier values may be numeric or character but must
#'   be unique. If not provided, the output circle identifiers will be the row
#'   numbers of the input circle data.
#'   
#' @return A data frame with columns: id, x, y; where id is the unique integer
#'   identifier for each circle. If no size values in the input \code{layout}
#'   data are positive, a data frame with zero rows will be returned.
#' 
#' @seealso \code{\link{circleVertices}}
#'   
#' @examples
#' xmax <- 100
#' ymax <- 100
#' rmin <- 10
#' rmax <- 20
#' N <- 20
#' 
#' ## Random centre coordinates and radii
#' layout <- data.frame(id = 1:N,
#'                      x = runif(N, 0, xmax), 
#'                      y = runif(N, 0, ymax), 
#'                      radius = runif(N, rmin, rmax))
#' 
#' ## Get data for circle vertices
#' verts <- circleLayoutVertices(layout, idcol=1, xysizecols=2:4,
#'                               sizetype = "radius")
#' 
#' \dontrun{
#' library(ggplot2)
#' 
#' ## Draw circles annotated with their IDs
#' ggplot() + 
#'   geom_polygon(data = verts, aes(x, y, group = id), 
#'                fill = "grey90", 
#'                colour = "black") +
#'                
#'   geom_text(data = layout, aes(x, y, label = id)) + 
#'   
#'   coord_equal() +
#'   theme_bw()
#' }
#' 
#' @export
#' 
circleLayoutVertices <- function(layout, npoints=25, xysizecols=1:3, 
                                 sizetype = c("radius", "area"),
                                 idcol=NULL) {

  if (is.matrix(layout)) layout <- as.data.frame(layout)
  checkmate::assert_data_frame(layout, min.cols = 3)
  
  checkmate::assert_int(npoints)
  
  if (is.numeric(xysizecols)) {
    checkmate::assert_integer(xysizecols, lower = 1, upper = ncol(layout), any.missing = FALSE, len = 3)
  } else if (is.character(xysizecols)) {
    checkmate::assert_character(xysizecols, any.missing = FALSE, len = 3)
    checkmate::assert_subset(xysizecols, colnames(layout))
  } 
  
  xcol <- xysizecols[1]
  ycol <- xysizecols[2]
  sizecol <- xysizecols[3]

  checkmate::assert_numeric(layout[[sizecol]], finite = TRUE, all.missing = FALSE)
  
  
  
  # Set any negative or missing sizes to zero
  ii <- is.null(layout[[sizecol]]) | is.na(layout[[sizecol]]) | layout[[sizecol]] < 0
  layout[[sizecol]][ii] <- 0
  
  # Check if there are any circles with positive sizes. If not, return
  # a zero-row data frame
  if (isTRUE(all.equal(layout[[sizecol]], rep(0, nrow(layout))))) {
    warning("All circles have zero or missing sizes")
    return(data.frame(id = integer(0), x = numeric(0), y = numeric(0)))
  }
  
  sizetype <- match.arg(sizetype)

  # Convert sizes to radii if provided as areas
  if (sizetype == "area") layout[[sizecol]] <- sqrt(layout[[sizecol]] / pi) 
  
  # Check circle ID values
  if (is.null(idcol)) {
    circle_ids <- 1:nrow(layout)
  } else if (is.numeric(idcol)) {
    checkmate::assert_int(idcol, lower = 1, upper = ncol(layout))
    circle_ids <- layout[[idcol]]
  } else {
    checkmate::assert_string(idcol, min.chars = 1)
    checkmate::assert_subset(idcol, colnames(layout))
    circle_ids <- layout[[idcol]]
  }
  
  # ID values should be non-missing and unique
  if (any(is.null(circle_ids) | is.na(circle_ids))) {
    stop("One or more of the specified ID values for circles are NULL or missing")
  } else if (length(circle_ids) > length(unique(circle_ids))) {
    stop("Not all of the specified ID values for circles are unique")
  }
  
  verts <- lapply(
    1:nrow(layout), 
    function(i) {
      df <- as.data.frame(
        circleVertices(layout[[i, xcol]], 
                       layout[[i, ycol]], 
                       layout[[i, sizecol]], 
                       npoints) )
      
      df$id <- circle_ids[i]
      df
    })
  
  do.call(rbind, verts)
}



#' Generate vertex coordinates for a circle
#' 
#' Generates vertex coordinates for a circle given its centre coordinates
#' and radius.
#' 
#' @param xc Value for centre X ordinate.
#' @param yc Value for centre Y ordinate.
#' @param radius Value for radius.
#' @param npoints The number of distinct vertices required.
#' 
#' @return A 2-column matrix of X and Y values. The final row is a copy
#'   of the first row to create a closed polygon, so the matrix has
#'   \code{npoints + 1} rows.
#' 
#' @seealso \code{\link{circleLayoutVertices}}
#' 
#' @export
#' 
circleVertices <- function(xc, yc, radius, npoints=25) {
  checkmate::assert_number(xc, finite = TRUE)
  checkmate::assert_number(yc, finite = TRUE)
  checkmate::assert_number(radius, finite = TRUE, lower = 0)
  
  a <- seq(0, 2*pi, length.out = npoints + 1)
  x <- xc + radius * cos(a)
  y <- yc + radius * sin(a)
  m <- cbind("x" = x, "y" = y)
}


#' Generate a set of circle vertices suitable for plotting
#' 
#' This function is deprecated and will be removed in a future release. 
#' Please use \code{circleLayoutVertices} instead.
#' 
#' 
#' @param layout A matrix or data frame of circle data (x, y, radius). May contain
#'   other columns, including an optional ID column.
#'   
#' @param npoints The number of vertices to generate for each circle.
#' 
#' @param xyr.cols Indices or names of columns for x, y, radius (in that order).
#'   Default is columns 1-3.
#' 
#' @param id.col Optional index or name of column for circle IDs in output. 
#'   If not provided, the output circle IDs will be the row numbers of
#'   the input circle data.
#' 
#' @return A data frame with columns: id, x, y; where id is the unique
#'   integer identifier for each circle.
#' 
#' @seealso \code{\link{circleLayoutVertices}} \code{\link{circleVertices}}
#' 
#' @export
#' 
circlePlotData <- function(layout, npoints = 25, xyr.cols = 1:3, id.col = NULL) {
  
  circleLayoutVertices(layout = layout, 
                       npoints = npoints, 
                       xysizecols = xyr.cols, 
                       sizetype = "radius", 
                       idcol = id.col)
}

