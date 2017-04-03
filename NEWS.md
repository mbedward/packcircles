# NEWS packcircles

Version 0.2.0 (coming soon)

* Added `circleProgressiveLayout` which deterministically places each circle in 
turn such that it is externally tangent to two previously placed circles while 
avoiding overlaps.

* Renamed `circleLayout` to `circleRepelLayout`. The previous name remains for
backwards compatibility but is deprecated.

* Removed `gridExtra` from suggested packages. It was previously used in the vignettes.
