# dynwrap 1.2.1 (2020-05-11)

* BUG FIX `project_trajectory()`: Correctly pass parameters.

* MINOR CHANGES `select_waypoints()`: Do not recompute waypoints if trajectory already contains some.

* MINOR CHANGES `convert_progressions_to_milestone_percentages()`: Solve tapply issues ahead of dplyr 1.0 release.

## Test environments
* local Fedora install, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes
