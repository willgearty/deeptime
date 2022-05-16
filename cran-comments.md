## Resubmission
This is a resubmission.

* Removed tidyverse dependency from tests

## Test environments (with Github Actions)

* windows (x86_64-w64-mingw32), R 4.2.0
* Mac OS X (x86_64-apple-darwin17.0), R 4.2.0
* ubuntu 20.04 (x86_64-pc-linux-gnu), R 4.2.0 and devel (r82335)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
   
 * Examples with CPU (user + system) or elapsed time > 5s
   
   The identified example takes up to 8 seconds depending on the OS (but less than 5 seconds on Ubuntu).
   The example is necessary to exemplify the functionality of the function.

## Downstream dependencies
Checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * Found 0 new problems
 * Failed to check 0 packages
