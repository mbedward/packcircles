# packcircles 0.3.6 (2023-09-05)

* Fix: Minor change to conform to CRAN package doc conventions.

# packcircles 0.3.5 (2022-11-23)

* Fix: Updated ggiraph dependency to version 0.8.4 to fix error when building
  one of the vignettes.

# packcircles 0.3.4 (2020-12-10)

* Fix: Minor updates to comply with R extensions convention for using packages 
  listed under 'Suggests'.

# packcircles 0.3.3 (2018-09-18)

* Fix: Minor updates to comply with the planned introduction of 
  STRICT_R_HEADERS in Rcpp.

# packcircles 0.3.2 (2018-04-28)

* Minor change in `pads_circle_pack.cpp` (graph-based circle packing) 
  to address a CRAN warning.

# packcircles 0.3.1 (2018-01-09)

* Fixed a problem where function circleProgressiveLayout sometimes returned
  a layout with overlapping circles. Thanks to Jeffrey Lewis for reporting
  and Peter Menzel for code changes.

# packcircles 0.3.0 (2017-11-24)

* Feature: Added `circleRemoveOverlaps` which takes a set of circles and
  identifies a subset of non-overlapping circles. The function can also be set
  to allow some degree of overlap or require space between circles. The
  function uses either a fast heuristic algorithm (several choices) or linear
  programming (requires package lpSolve).
  
* Fix: Replaced use of `sprintf` with `snprintf` in `pads_circle_pack.cpp`
  (graph-based circle packing) to address CRAN warning.

# packcircles 0.2.0 (2017-04-05)

* Feature: Added `circleProgressiveLayout` which deterministically places each
  circle in turn such that it is externally tangent to two previously placed 
  circles while avoiding overlaps.

* Feature: Replaced `circleLayout` with a new function `circleRepelLayout`. 
  The original function is retained for backwards compatibility but is 
  deprecated and will be removed in a future release.
  __Important note:__ the new function accepts circles sizes as either areas or 
  radii. The default is area, unlike `circleLayout` which assumed sizes were 
  radii. The type of size value can be specified using the `sizetype` argument.
  
* Feature: Replaced `circlePlotData` with a new function `circleLayoutVertices`.
  The original function is retained for backwards compatibility but is 
  deprecated and will be removed in a future release. The new function has a 
  `sizetype` argument to specify whether the input circle sizes are areas or 
  radii. By default, radius is assumed as this matches the output of the layout
  functions but I realize it's a bit confusing.

* Internal: Removed `gridExtra` from suggested packages. 

* Internal: Added `ggiraph` (used for vignette).

* Docs: Re-wrote the introductory vignette and added a new vignette for 
  `circleProgressiveLayout`.

* Docs: Edited function docs and examples.

* Internal: Added file for native routine registration as now required by CRAN.
