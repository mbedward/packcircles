# packcircles
R package for circle packing.

[![Travis Build Status](https://travis-ci.org/mbedward/packcircles.svg?branch=master)](https://travis-ci.org/mbedward/packcircles)

This package provides functions to find non-overlapping arrangements of circles.

The function `circleLayout` attempts to arrange a set of circles of specified
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
hierarchical data by circle packing* by Weixin Wang, Hui Wang, Guozhong Dai, and
Hongan Wang. Published in *Proceedings of the SIGCHI Conference on Human Factors
in Computing Systems*, 2006, pp. 517-520.
[ACM](https://dl.acm.org/citation.cfm?id=1124851).


The function `circleGraphLayout` is an initial Rcpp port of an algorithm described by
[Collins and Stephenson (2003)](http://www.sciencedirect.com/science/article/pii/S0925772102000998)
to find an arrangement of circles which corresponds to a graph of desired circle tangencies.
The implementation is based on a Python version by David Eppstein (see CirclePack.py in
the [PADS](http://www.ics.uci.edu/~eppstein/PADS/) library.

To install:

* the latest released version: `install.packages("packcircles")`
* the latest development version: `install_github("mbedward/packcircles")`

Share and enjoy!

