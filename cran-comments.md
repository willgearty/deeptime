## Test environments (with Github Actions)

* Windows 10.0.20348 (x86_64-w64-mingw32): R 4.2.2
* Mac OS X 12.6.2 (x86_64-apple-darwin17.0): R 4.2.2
* Ubuntu 20.04.1 (x86_64-pc-linux-gnu): R 4.1.3, 4.2.2, and devel (r83635)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies

Checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package:

 * RevGadgets (1.1.0)
 * EvoPhylo (0.3.2)
 * palaeoverse (1.1.0)

###Dependency check results:
All packages that I could install passed except:

 * palaeoverse: Some of the snapshot tests fail due to a dependency on data from an old version of the deeptime package. I am one of the co-authors of the palaeoverse package and have already removed this dependency in the development version of the package. A new version of the palaoeverse package will be submitted to CRAN in the coming weeks. Until then, this test dependency should not have any effect on the use of the package by end users.
