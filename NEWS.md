# dynwrap 1.2 (unreleased)

## Documentation

* Added examples to all functions

## New features

* Improved RNA velocity handling:
  - Not all features need to be present in the projected expression, allowing integration with standard velocyto.R pipelines

# dynwrap 1.1.4 (27-06-2019)

* BUG FIX: Fixed #142 where the error message was truncated

# dynwrap 1.1.3 (05-06-2019)

* MINOR CHANGE `add_dimred()`: Add a separate argument for specifying the projected dimred rather than
  expecting the projected dimred to be passed as additional columns in `dimred`.

* BUG FIX: Fix for dynverse/dyno#52, do specify whether or not to use optional priors when passed.

# dynwrap 1.1.2 (08-05-2019)

* FEATURE: Add leaves_n as prior information

# dynwrap 1.1.1 (08-05-2019)

* BUG FIX: Fixed bug for directed geodesic distances with disconnected gra    phs, dynverse/dynplot#37

# dynwrap 1.1 (07-05-2019)

* FEATURE: RNA velocity data can now be included in the wrapper

* FEATURE: RNA velocity projected expression can now be given to methods

* FEATURE: Added `orient_topology_to_velocity()` to orient the edges of a trajectory based on the velocity vectors

* FEATURE: `calculate_geodesic_distances()` now has a `directed` parameter, with which you can calculate directed geodesic distances. Unreachable cells will receive an infinite distance

* FEATURE: Added `projected_trajectory()` (ported from dynplot). `add_dimred()` will now by default also add a projection of the trajectory to the data

# dynwrap 1.0.1 (09-04-2019)

* MINOR CHANGE: `infer_trajectory(..., debug = TRUE)` will now return commands for both entering the container and running the `dyncli::main()` function inside the container

* MINOR CHANGE: Make `create_ti_method_definition()` actually work

* DOCUMENTATION: Added examples for each trajectory wrapper

* DOCUMENTATION: Added vignette discussing wrapper types

* DOCUMENTATION: Added vignette discussing `create_ti_method_definition()`

* BUG FIX: Fixed bug where `sink()` was not removed after interruption

# dynwrap 1.0.0 (28-03-2019)

* MAJOR CHANGE: Add support for Singularity 3.0, drop support for previous 
  releases of Singularity and singularity-hub.
  
* MAJOR CHANGE: dynwrap now always works with sparse count and expression matrices

* FEATURE: Add `create_ti_method_definition()` to create a definition from a local script.

* DOCUMENTATION: Major update of all documentation for dynbenchmark publication

* MINOR CHANGE: Rename `compute_tented_geodesic_distances()` to `calculate_geodesic_distances()`

* MINOR CHANGE: Harmonisation of function arguments to either `dataset` or `trajectory`

# dynwrap 0.3.1.2 (01-02-2019)

* BUG FIX: `simplify_replace_edges()` would sometimes swap edges in milestone network around, but forget
  invert percentages.
  
* BUG FIX: Close sinks when interupting the R process

* MINOR CHANGE: Work with new babelwhale, which includes support for singularity 3.0

# dynwrap 0.3.1.1 (17-12-2018)

* CLEAN UP: Removed helper functions that are not required any more:
  `get_env_or_null()`, `read_rds_or_null()` and `print_processx()`.
  
* MINOR CHANGE: remove requirement that `milestone_ids` and `cell_ids` cannot overlap

# dynwrap 0.3.1 (19-11-2018)

* HOTFIX: Use `utils::data()` to get access to `priors`.

# dynwrap 0.3.0 (19-11-2018)

* MINOR CHANGE: Added metadata on the different wrapper types implemented in dynwrap.

* CLEAN UP: Removed `plot_fun` argument from `create_ti_method()`.

* MINOR CHANGE: Replaced `mc_cores` with more flexible `map_fun`.

* MINOR CHANGE: Renamed `create_ti_method()` to `create_ti_method_r()`, 
  and `create_ti_method_with_container()` to `create_ti_method_container()`.

* CLEAN UP: Drastically reworked `create_ti_method_r()` and `create_ti_method_container()`, 
  and the underlying functions for executing a method on a dataset. 
  
* CLEAN UP: Remove `parse_parameter_definition()` and thereby dependency on ParamHelpers.

# dynwrap 0.2.0 (29-10-2018)

* BUG FIX: Fixed incorrect calculation of milestone percentage during trajectory simplification.
  Occurs only in a rare edge case, namely when the order of the milestones in the milestone network
  is very different from the order of the milestone ids (0475e94).

* BUG FIX: Fixed suggested dependencies not being installed in the dynwrap containers (#100).

* FEATURE REMOVAL: Removed feather data format because it's not being used and creates dependency issues every now and again.

* BUG FIX: `devtools:::shim_system.file()` has been moved to `pkgload:::shim_system.file()`.

* TESTING: Solved issue with the unit tests by not using any helpers.

* MINOR CHANGE: Have docker images build from dynwrap@devel.

* BUG FIX: Remove `option(echo = FALSE)` from .Rprofile in recipes because some packages directly rely 
  on standard output from R, so printing the command wreaks havoc.

# dynwrap 0.1.0 (07-03-2018)

* INITIAL RELEASE: dynwrap, functionality for containerised trajectory inference.
 - Wrap the input data of a trajectory inference method, such as expression and prior information
 - Run a trajectory inference method in R, in a docker container or a singularity container
 - Wrap the output of a trajectory inference method, such as the pseudotime, a clustering or a branch network, and convert it into a common trajectory model
 - Further postprocess the trajectory model, such as labelling the milestones and rooting the trajectory
