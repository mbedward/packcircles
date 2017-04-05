#' packcircles: Simple algorithms for circle packing
#' 
#' This package provides several algorithms to find non-overlapping
#' arrangements of circles:
#' \describe{
#'  \item{circleRepelLayout}{Arranges circles within a bounding rectangle
#'    by pairwise repulsion.}
#'  \item{circleProgressiveLayout}{Arranges circles in an unbounded area
#'    by progressive placement. This is a very efficient algorithm that can
#'    handle large numbers of circles.}
#'  \item{circleGraphLayout}{Finds an arrangement of circles conforming to
#'    a graph specification.}
#' }
#' 
#' @docType package
#' @name packcircles
#' 
#' @importFrom Rcpp sourceCpp
#' @useDynLib packcircles
#' 
NULL
