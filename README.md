
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://img.shields.io/travis/dynverse/dynwrap.svg?logo=travis)](https://travis-ci.org/dynverse/dynwrap)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/dynverse/dynwrap?branch=master&svg=true)](https://ci.appveyor.com/project/dynverse/dynwrap)
[![codecov](https://codecov.io/gh/dynverse/dynwrap/branch/master/graph/badge.svg)](https://codecov.io/gh/dynverse/dynwrap)
<img src="man/figures/logo.png" align="right" />

# Tools for inferring and wrapping single-cell trajectories

**dynwrap** contains the code for a common model of single-cell
trajectories. The package can:

  - Wrap the input data of a trajectory inference method, such as
    expression and prior information
  - Run a trajectory inference method in R, in a docker container or a
    singularity container
  - Wrap the output of a trajectory inference method, such as the
    pseudotime, a clustering or a branch network, and convert it into a
    common trajectory model
  - Further postprocess the trajectory model, such as labelling the
    milestones and rooting the trajectory

![common trajectory model](man/figures/trajectory_model.png)

dynwrap was used to wrap 50+ trajectory inference method within docker
containers in [dynmethods](https://github.com/dynverse/dynmethods).

![](man/figures/overview_wrapping_v2.png)

The advantage of using a common model is that it allows:

  - Comparison between a prediction and a gold standard, eg. using
    [dyneval](https://www.github.com/dynverse/dyneval)
  - Comparing two predictions
  - Easily visualise the trajectory, eg. using
    [dynplot](https://www.github.com/dynverse/dynplot)
  - Extracting relevant features/genes, eg. using
    [dynfeature](https://www.github.com/dynverse/dynfeature)

## Latest changes

Check out `news(package = "dynwrap")` or [NEWS.md](inst/NEWS.md) for a
full list of
changes.

<!-- This section gets automatically generated from inst/NEWS.md, and also generates inst/NEWS -->

### Latest changes in dynwrap 0.3.1.2 (unreleased)

  - BUG FIX: `simplify_replace_edges()` would sometimes swap edges in
    milestone network around, but forget invert percentages.

### Latest changes in dynwrap 0.3.1.1 (17-12-2018)

  - CLEAN UP: Removed helper functions that are not required any more:
    `get_env_or_null()`, `read_rds_or_null()` and `print_processx()`.

  - MINOR CHANGE: remove requirement that `milestone_ids` and `cell_ids`
    cannot overlap
