#' Generate a set of circle vertices suitable for plotting
#' 
#' Given a matrix or data frame for a circle layout, with columns for centre x-y
#' coordinates and sizes, this function generates a data set of vertices which 
#' can then be used with ggplot or base graphics functions.
#' 
#' @note \strong{Input sizes are assumed to be radii}. This is slightly confusing
#'   because the layout functions \code{circleRepelLayout} and 
#'   \code{circleProgressiveLayout} treat their input sizes as areas by default.
#'   To be safe, you can always set the \code{sizetype} argument explicitly
#'   for both this function and layout functions.
#'   
#' @param layout A matrix or data.frame of circle data (x, y, size). May also 
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
#' @param idcol Optional index or name of column for circle identifiers. These 
#'   may be numeric or character but must be unique. If not provided, the output
#'   circle IDs will be the row numbers of the input circle data.
#'   
#' @return A data.frame with columns: id, x, y; where id is the unique integer 
#'   identifier for each circle.
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

  sizetype <- match.arg(sizetype)
  
  layout <- as.data.frame(layout)
  xcol <- xysizecols[1]
  ycol <- xysizecols[2]
  sizecol <- xysizecols[3]
  
  if (is.null(idcol))
    ids <- 1:nrow(layout)
  else
    ids <- layout[[idcol]]
  
  if (sizetype == "area") layout[[sizecol]] <- sqrt(layout[[sizecol]] / pi) 
  
  verts <- lapply(
    1:nrow(layout), 
    function(i) {
      df <- as.data.frame(
        circleVertices(layout[[i, xcol]], 
                       layout[[i, ycol]], 
                       layout[[i, sizecol]], 
                       npoints) )
      
      df$id <- ids[i]
      df
    })
  
  do.call(rbind, verts)
}



#' Generate vertex coordinates for a circle
#' 
#' Generates vertex coordinates for a circle given its centre coordinates
#' and radius.
#' 
#' @param xc Circle centre X ordinate.
#' @param yc Circle centre Y ordinate.
#' @param radius Circle radius.
#' @param npoints Number of distinct vertices required.
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
#' @param layout A matrix or data.frame of circle data (x, y, radius). May contain
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
#' @return A data.frame with columns: id, x, y; where id is the unique
#'   integer identifier for each circle.
#' 
#' @seealso \code{\link{circleLayoutVertices}} \code{\link{circleVertices}}
#' 
#' @export
#' 
circlePlotData <- function(layout, npoints = 25, xyr.cols = 1:3, id.col = NULL) {
  
  circleLayoutVertices(layout, npoints, 
                       xysizecols = xyr.cols, 
                       sizetype = "radius", 
                       idcol = id.col)
}

