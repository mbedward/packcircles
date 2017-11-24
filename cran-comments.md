## Summary

This submission adds new package features and also addresses the following warning
received by email from CRAN maintainers:

    Found the following significant warnings:
    pads_circle_pack.cpp:233:6: warning: ‘sprintf’ may write a terminating nul
    path the end of the destination [-Wformat-overflow=] 
  
Instances of 'sprintf' have been replaced by 'snprintf' to ensure safety.

## Test environments

  * Mac OSX 10.11.6
  * Ubuntu 14.04 (Travis CI)
  * win-builder (release and devel)

## R CMD check results

There were no ERRORs or WARNINGs.

There were no NOTES (other than maintainer details).

## Downstream dependencies

There are currently no downstream dependencies for this package.
