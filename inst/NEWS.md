
# dynwrap 0.3.0 (unreleased)


# dynwrap 0.2.0 (29-10-2018)

* BUG FIX: Fix incorrect calculation of milestone percentage during trajectory simplification.
  Occurs only in a rare edge case, namely when the order of the milestones in the milestone network
  is very different from the order of the milestone ids (0475e94).

* BUG FIX: Fix suggested dependencies not being installed in the dynwrap containers (#100).

* FEATURE REMOVAL: Remove feather data format because it's not being used and creates dependency issues every now and again.

* BUG FIX: `devtools:::shim_system.file()` has been moved to `pkgload:::shim_system.file()`.

* TESTING: Solve issue with the unit tests by not using any helpers.

* MINOR CHANGE: Have docker images build from dynwrap@devel.

* BUG FIX: Fix weird bug in Dockerfiles / recipes where minqa can't be installed when .Rprofile is not empty.

# dynwrap 0.1.0 (07-03-2018)

* INITIAL RELEASE: dynwrap, functionality for containerised trajectory inference.
 - Wrap the input data of a trajectory inference method, such as expression and prior information
 - Run a trajectory inference method in R, in a docker container or a singularity container
 - Wrap the output of a trajectory inference method, such as the pseudotime, a clustering or a branch network, and convert it into a common trajectory model
 - Further postprocess the trajectory model, such as labelling the milestones and rooting the trajectory
