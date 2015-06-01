#' Generate plotting data for circles.
#' 
#' Takes a 3-column matrix or data.frame of circle data (centre x, centre y, 
#' radius) and generates a data.frame of plotting data to use with ggplot.
#' 
#' @param xyr matrix or data.frame of circle data (x, y, radius)
#' @param npoints number of vertices to generate for each circle
#' @param indices or names of columns for x, y, radius (in that order).
#'   Default is columns 1-3.
#' @param optional index or name of column for circle IDs in output. 
#'   Default is to use input row numbers as ID.
#' 
#' @return A data.frame with columns: id, x, y; where id is an
#'   integer identifier for each circle.
#' 
#' @seealso \code{\link{circleVertices}}
#' 
#' @examples
#' \dontrun{
#' ## draw some random circles with ggplot
#' library(ggplot2)
#' 
#' xmax = 100
#' ymax = 200
#' rmax <- 20
#' N <- 100
#' 
#' ## Random centre coordinates and radii
#' xyr <- data.frame(x=runif(N, 0, xmax), 
#'                   y=runif(N, 0, ymax), 
#'                   r=rbeta(N, 1, 5) * rmax)
#'
#' ## Get data for circle vertices
#' dat <- circlePlotData(xyr)
#' 
#' ## Draw them
#' ggplot(dat, aes(x, y, group=id)) + 
#'   geom_path(colour="steelblue") + 
#'   coord_equal(xlim=c(0, xmax), ylim=c(0, ymax)) +
#'   theme_bw()
#' }
#' 
#' @export
#'
circlePlotData <- function(xyr, npoints=25, xyr.cols=1:3, id.col=NULL) {
  if (is.null(id.col))
    ids <- 1:nrow(xyr)
  else
    ids <- xyr[, id.col]
  
  do.call("rbind", lapply(ids, function(i) {
    df <- as.data.frame(
      circleVertices(xyr[i, xyr.cols[1]], 
                     xyr[i, xyr.cols[2]], 
                     xyr[i, xyr.cols[3]], 
                     npoints) )
    df$id <- i
    df
  }))
}
