## Test environments

* local Windows 11 x64, R 4.5.2, `R CMD check --as-cran`
* R-hub platforms: Linux, macOS, Windows, Fedora/GCC checks used during pre-submission testing

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission.
* `cmdstanr` is listed in `Suggests` and is available from the Stan R-universe repository declared in `Additional_repositories`. It is used for fitting Stan models; the package does not require `cmdstanr` to load.
* On the local Windows check, R reported `unable to verify current time` when checking for future file timestamps. This appears to be specific to the local check environment.

## Additional notes

The package includes a precomputed object for the vignette so examples and vignettes do not run the long Stan model fit during `R CMD check`.
