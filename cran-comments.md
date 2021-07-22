## Resubmission
This is a resubmission. In this version I have:

* Expanded the Description field in the DESCRIPTION file.
* Added return values/details (`\value{}`) to the documentation for `disparity_through_time()` and `panel.disparity()`.
* Replaced `\dontrun{}` with `\donttest{}` (the example can take more than 5s depending on the OS).

## Test environments (with Github Actions)

* windows (x86_64-w64-mingw32), R 4.1.0
* Mac OS X (x86_64-apple-darwin17.0), R 4.1.0
* ubuntu 20.04 (x86_64-pc-linux-gnu), R 4.1.0 and devel (r80647)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.