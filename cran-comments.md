This new major version adds several pieces of new functionality, one of which has significantly increased the size of the package. To attempt to offset this, I have moved two dependencies to Suggests (geomtextpath and phytools), the latter of which is quite heavy.

## Test environments (with Github Actions)

* Windows 10.0.20348 (x86_64-w64-mingw32): R 4.4.1
* Mac OS X 14.5 (aarch64-apple-darwin20): R 4.4.1
* Ubuntu 22.04.4 (x86_64-pc-linux-gnu): R 4.3.3, 4.4.1, and devel (r86969)

* All checks were run separately with ggplot2 3.5.0 and 3.5.1

## R CMD check results
There were no ERRORs or WARNINGs. There was one NOTE. The inclusion of new internal data has increased the "R" directory to between 2.7 MB and 4.0 MB (depending on OS). The inclusion of patterns in the vignette figures has increased the "doc" directory to between 1.2 MB and 2.2 MB (depending on OS). All folders individually remain less than 5 MB and the package tarball remains less than 10 MB.

## Downstream dependencies
Checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package:

 * EvoPhylo (0.3.2)
 * RevGadgets (1.2.1)
 * rmacrostrat (0.0.1)
 * rphylopic (1.4.0)

### Dependency check results:
All packages passed.
