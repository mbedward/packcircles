#' Generate plotting data for circles.
#' 
#' Given a matrix or data.frame of circle layout data (circle centres, radii 
#' and optional IDs), generates a data.frame of plotting data to use with ggplot.
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
#' @seealso \code{\link{circleVertices}}
#' 
#' @examples
#' \dontrun{
#' ## draw some random circles with ggplot
#' library(ggplot2)
#' 
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
#'                      r = runif(N, rmin, rmax))
#'
#' ## Get data for circle vertices
#' plotdat <- circlePlotData(layout, id.col=1, xyr.cols=2:4)
#' 
#' ## Draw circles annotated with their IDs
#' ggplot() + 
#'   geom_polygon(data=plotdat, aes(x, y, group=id), fill=NA, colour="black") +
#'   geom_text(data=layout, aes(x, y, label=id)) + 
#'   coord_equal() +
#'   theme_bw()
#' }
#' 
#' @export
#'
circlePlotData <- function(layout, npoints=25, xyr.cols=1:3, id.col=NULL) {
  if (is.null(id.col))
    ids <- 1:nrow(layout)
  else
    ids <- layout[, id.col]
  
  do.call("rbind", lapply(1:nrow(layout), function(i) {
    df <- as.data.frame(
      circleVertices(layout[i, xyr.cols[1]], 
                     layout[i, xyr.cols[2]], 
                     layout[i, xyr.cols[3]], 
                     npoints) )
    df$id <- ids[i]
    df
  }))
}
