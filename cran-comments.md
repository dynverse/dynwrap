# dynwrap 1.2.2

* MAJOR CHANGE `convert_milestone_percentages_to_progressions()`: Rewrite implementation to attain significant speedup.

* MINOR CHANGE `infer_trajectory()`: Infer command (Rscript/python) from file extension if possible and avoid using
  shebang to execute script, because R CMD check for R 4.0 puts Rscript and R dummy executables on the path before 
  R CMD check. This means `#!/usr/bin/env Rscript` does not work anymore.
  
* MINOR CHANGE `add_feature_importance()`: Add a function for adding feature importance scores to a trajectory.

* BUG FIX `project_waypoints()`: Rename milestone waypoints such that the names are unique.

* BUG FIX `infer_trajectory()`: Fix error message printing.

* BUG FIX: `dyndimred` is used conditionally.

* BUG FIX: `wrap_expression()` and `add_expression()`: Do not override feature_info when it already exists in dataset.

## Test environments
* local Fedora install, R 4.0
* ubuntu 20.04, mac os x, windows (on github actions), R 4.0
* win-builder (devel and release)

## R CMD check results

```
── R CMD check results ────────────────────────────────────── dynwrap 1.2.2 ────
Duration: 2m 53.4s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```


## Reverse dependencies

A reverse dependency check was run on all downstream dependencies.
(Summary at [revdep/README.md](revdep/README.md)). No new problems were found.

```
> revdepcheck::revdep_check(timeout = as.difftime(60, units = "mins"), num_workers = 30)
── INIT ────────────────────────────────────────────────────────────────────────
── INSTALL ─────────────────────────────────────────────────────────────────────
Installing CRAN version of dynwrap
also installing the dependencies ‘prettyunits’, ‘rprojroot’, ‘RcppParallel’, ‘RcppArmadillo’, ‘pryr’, ‘pkgbuild’, ‘rstudioapi’, ‘diffobj’, ‘rematch2’, ‘utf8’, ‘ellipsis’, ‘generics’, ‘lifecycle’, ‘R6’, ‘rlang’, ‘tidyselect’, ‘vctrs’, ‘desc’, ‘proxyC’, ‘Rcpp’, ‘remotes’, ‘carrier’, ‘pkgconfig’, ‘ps’, ‘cli’, ‘clipr’, ‘hms’, ‘BH’, ‘cpp11’, ‘plyr’, ‘brio’, ‘callr’, ‘pkgload’, ‘praise’, ‘waldo’, ‘withr’, ‘fansi’, ‘pillar’, ‘assertthat’, ‘babelwhale’, ‘crayon’, ‘dplyr’, ‘dynutils’, ‘dynparam’, ‘igraph’, ‘purrr’, ‘processx’, ‘readr’, ‘reshape2’, ‘testthat’, ‘tibble’, ‘tidyr’

Installing DEV version of dynwrap
Installing 42 packages: digest, assertthat, vctrs, utf8, rlang, lifecycle, fansi, ellipsis, crayon, cli, Rcpp, pkgconfig, pillar, R6, generics, cpp11, tidyselect, tibble, purrr, magrittr, dplyr, pryr, RcppArmadillo, RcppParallel, rprojroot, ps, remotes, proxyC, desc, plyr, BH, hms, clipr, tidyr, dynutils, carrier, processx, reshape2, readr, igraph, dynparam, babelwhale
── CHECK ───────────────────────────────────────────────────────────────────────
✓ SCORPIUS 1.0.7                         ── E: 0     | W: 0     | N: 0                                                                                                                                                                   
✓ dyngen 1.0.0                           ── E: 0     | W: 0     | N: 1                                                                                                                                                                   
OK: 2                                                                                                                                                                                                                                  
BROKEN: 0
Total time: 4 min
── REPORT ──────────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Writing CRAN report to 'revdep/cran.md'
```
