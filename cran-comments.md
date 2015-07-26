## Resubmission

Previous submission received comment:
Thanks, we see:
 * Copyright (c) 2002-2015, David Eppstein
in one of your files. So apparently you do not list all authors of the cde in your DESCRIPTION file. 
Please carefully add all authors and copyright holders.

In this version, I have added David Eppstein to the description file as an author with a comment that he
is the original author of Python code ported to C++ for this package. This package uses the same license
(MIT) as David's original code (http://www.ics.uci.edu/~eppstein/PADS/ABOUT-PADS.txt). The URL to the
original Python code is included in the header of the relevant C++ source file (src/pads_circle_pack.cpp).
I have contacted David and have his permission for the above use and credit of his work. 


## Test environments
* Ubuntu 12.04 (Travis CI)
* win-builder (release and devel)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Michael Bedward <michael.bedward@gmail.com>’
  New submission
  
* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
