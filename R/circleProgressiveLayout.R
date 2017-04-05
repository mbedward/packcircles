#' Progressive layout algorithm
#' 
#' Arranges a set of circles, which are denoted by their sizes, by consecutively 
#' placing each circle externally tangent to two previously placed circles while 
#' avoiding overlaps. 
#' 
#' Based on an algorithm described in the paper:
#' \emph{Visualization of large hierarchical data by circle packing}
#' by Weixin Wang, Hui Wang, Guozhong Dai, and Hongan Wang. Published
#' in \emph{Proceedings of the SIGCHI Conference on Human Factors in Computing Systems},
#' 2006, pp. 517-520 \url{https://dl.acm.org/citation.cfm?id=1124851}
#' 
#' The implementation here was adapted from a version written in C by Peter Menzel:
#' \url{https://github.com/pmenzel/packCircles}.
#' 
#' @param x Either a vector of circle sizes, or a matrix or data frame
#'   with one column for circle sizes.
#'   
#' @param sizecol The index or name of the column in \code{x} for circle sizes.
#'   Ignored if \code{x} is a vector.
#'   
#' @param sizetype The type of size values: either \code{"area"} (default) 
#'   or \code{"radius"}. May be abbreviated.
#' 
#' @return A data frame with columns: x, y, radius. If any of the input size values
#'   were non-positive or missing, the corresponding rows of the output data frame
#'   will be filled with \code{NA}s.
#'   
#' @examples
#' areas <- sample(c(4, 16, 64), 100, rep = TRUE, prob = c(60, 30, 10))
#' packing <- circleProgressiveLayout(areas)
#'
#' \dontrun{
#' 
#' # Graph the result with ggplot
#' dat.gg <- circleLayoutVertices(packing)
#' 
#' ggplot(data = dat.gg, aes(x, y, group = id)) +
#'   geom_polygon(colour = "black", fill = "grey90") +
#'   coord_equal() +
#'   theme_void()
#' 
#' }
#' 
#' @export
#' 
circleProgressiveLayout <- function(x, sizecol = 1, sizetype = c("area", "radius")) {
  sizetype = match.arg(sizetype)
  
  if (is.matrix(x)) {
    sizes <- as.numeric(x[, sizecol])
  }
  else if (is.data.frame(x)) {
    sizes <- as.numeric(x[[sizecol]])
  }
  else {
    sizes <- as.numeric(x)
  }
  
  if (any(sizes <= 0, na.rm = TRUE)) {
    sizes[ sizes <= 0 ] <- NA
  }
  missing <- is.na(sizes) | is.nan(sizes)
  
  if (all(missing)) stop("all sizes are missing and/or non-positive")
  
  if (any(missing)) warning("missing and/or non-positive sizes will be ignored")

  radii <- sizes[ !missing ]
  if (sizetype == "area") radii <- sqrt(radii / pi)
  
  res <- do_progressive_layout(radii)
  
  if (any(missing)) {
    out <- matrix(NA_real_, nrow = length(sizes), ncol = 3)
    colnames(out) <- colnames(res)
    out[!missing, ] <- as.matrix(res)
    as.data.frame(out)
  } 
  else {
    res
  }
}
