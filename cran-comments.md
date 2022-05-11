## Resubmission
This is a resubmission.

* The gsloid package is now back on CRAN.

## Test environments (with Github Actions)

* windows (x86_64-w64-mingw32), R 4.2.0
* Mac OS X (x86_64-apple-darwin17.0), R 4.2.0
* ubuntu 20.04 (x86_64-pc-linux-gnu), R 4.2.0 and devel (r82335)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTES:

 * Package suggested but not available for checking: ‘gsloid’
   
   gsloid is only used for tests and these tests are skipped if the package is not available.
   Furthermore, this package appears to be under active development and should be restored to CRAN soon.
   
 * Examples with CPU (user + system) or elapsed time > 5s
   
   The identified example takes up to 7 seconds depending on the OS (but less than 5 seconds on Ubuntu).
   The example is necessary to exemplify the functionality of the function.

## Downstream dependencies
Checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * Found 0 new problems
 * Failed to check 0 packages
