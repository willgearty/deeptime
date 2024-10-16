This minor version adds several new features.

## Test environments (with Github Actions)

* Windows 10.0.20348 (x86_64-w64-mingw32): R 4.4.1
* Mac OS X 14.7 (aarch64-apple-darwin20): R 4.4.1
* Ubuntu 22.04.4 (x86_64-pc-linux-gnu): R 4.3.3, 4.4.1, and devel (r87233)

* All checks were run separately with ggplot2 3.5.0 and 3.5.1

## R CMD check results
There were no ERRORs or WARNINGs. There was one NOTE about install size, but this is similar to the current version on CRAN. The "R" directory is between 3.1 MB and 4.3 MB in size (depending on OS). The "doc" directory is between 1.8 MB and 2.9 MB in size (depending on OS). All folders individually remain less than 5 MB and the package tarball remains less than 10 MB.

## Downstream dependencies
Checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package:

 * EvoPhylo (0.3.2)
 * RevGadgets (1.2.1)
 * rmacrostrat (0.0.2)
 * rphylopic (1.5.0)

### Dependency check results:
All packages passed.
