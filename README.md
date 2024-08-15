<!-- badges: start -->
[![R-CMD-check](https://github.com/mbedward/packcircles/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mbedward/packcircles/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# packcircles
R package for circle packing. Algorithms to find arrangements of non-overlapping circles

This package provides functions to find non-overlapping arrangements of circles.

The function `circleRepelLayout` attempts to arrange a set of circles of specified
radii within a rectangle such that there is no-overlap between circles. 
The algorithm is adapted from an example written in Processing by Sean
McCullough (which no longer seems to be available online). It involves
iterative pair-repulsion, in which overlapping circles move away from each
other. The distance moved by each circle is proportional to the radius of the
other to approximate inertia (very loosely), so that when a small circle is
overlapped by a large circle, the small circle moves furthest. This process
is repeated iteratively until no more movement takes place (acceptable
layout) or a maximum number of iterations is reached (layout failure). To
avoid edge effects, the bounding rectangle is treated as a toroid. Each
circle's centre is constrained to lie within the rectangle but its edges are
allowed to extend outside.


The function `circleProgressiveLayout` arranges a set of circles, which are
denoted by their sizes, by consecutively placing each circle externally tangent
to two previously placed circles while avoiding overlaps. It was adapted from a
[version written in C](https://github.com/pmenzel/packCircles) by Peter Menzel.
The underlying algorithm is described in the paper: *Visualization of large
hierarchical data by circle packing* by 
[Weixin Wang et al. (2006)](https://doi.org/10.1145/1124772.1124851).


The function `circleRemoveOverlaps` takes an initial set of overlapping circles
and attempts to find a non-overlapping subset or, optionally, a subset with some
specified degree of overlap. Circle positions remain fixed. It provides several
fast heuristic algorithms to choose from, as well as two based on linear 
programming. For the latter, package lpSolve must be installed.


The function `circleGraphLayout` is an initial Rcpp port of an algorithm described by
[Collins and Stephenson (2003)](https://doi.org/10.1016/S0925-7721(02)00099-8)
to find an arrangement of circles which corresponds to a graph of desired circle tangencies.
The implementation is based on a Python version by David Eppstein (see CirclePack.py in
the [PADS](https://www.ics.uci.edu/~eppstein/PADS/) library.

To install:

* the latest released version: `install.packages("packcircles")`
* the latest development version (usually the same): `install_github("mbedward/packcircles")`

See also:

The [ggcirclepack](https://github.com/EvaMaeRey/ggcirclepack) package
which makes it easier to add circles created with at least one of the `packcircles` 
functions to ggplot2 graphs.

Share and enjoy!
