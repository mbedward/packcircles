# NEWS packcircles

Version 0.3.0 2017-11-24

* Added `circleRemoveOverlaps` which takes a set of circles and identifies a
  subset of non-overlapping circles. The function can also be set to allow
  some degree of overlap or require space between circles. The function uses
  either a fast heuristic algorithm (several choices) or linear programming
  (requires package lpSolve).
  
* Replaced use of `sprintf` with `snprintf` in `pads_circle_pack.cpp`
  (graph-based circle packing) to address CRAN warning.

Version 0.2.0 2017-04-05

* Added `circleProgressiveLayout` which deterministically places each circle in 
  turn such that it is externally tangent to two previously placed circles while 
  avoiding overlaps.

* Replaced `circleLayout` with a new function `circleRepelLayout`. 
  The original function is retained for backwards compatibility but is deprecated
  and will be removed in a future release.
  __Important note:__ the new function accepts circles sizes as either areas or 
  radii. The default is area, unlike `circleLayout` which assumed sizes were radii. 
  The type of size value can be specified using the `sizetype` argument.
  
* Replaced `circlePlotData` with a new function `circleLayoutVertices`.
  The original function is retained for backwards compatibility but is deprecated
  and will be removed in a future release.
  The new function has a `sizetype` argument to specify whether the input circle
  sizes are areas or radii. By default, radius is assumed as this matches the output
  of the layout functions but I realize it's a bit confusing.

* Removed `gridExtra` from suggested packages. Added `ggiraph` (used for vignette).

* Re-wrote the introductory vignette and added a new vignette for `circleProgressiveLayout`.

* Edited function docs and examples.

* Added file for native routine registration as now required by CRAN.
