# dynwrap 1.2.3

* MINOR CHANGE: Fixes to metadata (documentation, citation entry, broken links).

* MINOR CHANGE: Use alternative method for checking whether dynmethods is installed.

## Test environments
* local Fedora install, R 4.2
* ubuntu 22.04, mac os x, windows (on github actions), R 4.2
* win-builder (devel and release)

## R CMD check results

```
── R CMD check results ────────────────────────────────────── dynwrap 1.2.3 ────
Duration: 2m 2.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
```


## Reverse dependencies

A reverse dependency check was run on all downstream dependencies.
(Summary at [revdep/README.md](revdep/README.md)). 

```
── INIT ─────────────────────────────────────────────────── Computing revdeps ──
── INSTALL ─────────────────────────────────────────────────────── 2 versions ──
Installing CRAN version of dynwrap
also installing the dependencies ‘prettyunits’, ‘cpp11’, ‘lobstr’, ‘pryr’, ‘fs’, ‘carrier’, ‘plyr’, ‘babelwhale’, ‘dynutils’, ‘dynparam’, ‘reshape2’

Installing DEV version of dynwrap
Installing 29 packages: Rcpp, cpp11, utf8, fansi, prettyunits, progress, vctrs, cli, stringi, stringr, purrr, dplyr, lobstr, pryr, RcppArmadillo, RcppParallel, plyr, vroom, tidyr, dynutils, carrier, digest, fs, yaml, reshape2, readr, igraph, dynparam, babelwhale
── CHECK ───────────────────────────────────────────────────────── 4 packages ──
✔ dyngen 1.0.5                           ── E: 1     | W: 0     | N: 0                                                                                                                          
✔ dynplot 1.1.2                          ── E: 0-1   | W: 0     | N: 0                                                                                                                          
✔ SCORPIUS 1.0.8                         ── E: 0-1   | W: 0     | N: 0                                                                                                                          
✖ dynfeature 1.0.0                       ── E: 0-1   | W: 0     | N: 0  +1                                                                                                                      
OK: 3                                                                                                                                                                                         
BROKEN: 1
Total time: 2 min
── REPORT ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Writing CRAN report to 'revdep/cran.md'
```

Revdepcheck reports a newly broken notice, but this is actually a problem within dynfeature that should be fixed.
