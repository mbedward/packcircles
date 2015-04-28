# packcircles
R package for random circle packing.

This package provides a simple algorithm to arrange circles of arbitrary
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

## TODO

- Allow a specified amount of overlap.
- Unbounded circle packing where we allow the layout to expand as required.
- Other bounding shapes.
- Allow differential overlap, e.g. circles assigned to two groups, with some
  specified amount of overlap allowed between circles from different groups.
 


