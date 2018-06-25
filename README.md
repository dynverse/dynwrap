[![Build Status](https://img.shields.io/travis/dynverse/dynwrap.svg?logo=travis)](https://travis-ci.org/dynverse/dynwrap)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dynverse/dynwrap?branch=master&svg=true)](https://ci.appveyor.com/project/dynverse/dynwrap)
[![codecov](https://codecov.io/gh/dynverse/dynwrap/branch/master/graph/badge.svg)](https://codecov.io/gh/dynverse/dynwrap) <img src="man/figures/logo.png" align="right" />

# Tools for inferring and wrapping single-cell trajectories

**dynwrap** contains the code for a common model of single-cell trajectories. The package can

* Wrap the input data of a TI method, such as expression and prior information
* Run a TI method locally or in a docker
* Wrap the output of a TI method, such as the pseudotime, a clustering or a branch network, and convert it into a common trajectory model
* Further postprocess the output, such as labelling milestones and rooting the trajectory

![](man/figures/overview_wrapping_v1.svg)

The advantage of using a common model is that it allows

* Comparison between a prediction and a gold standard, eg. using [dyneval](https://www.github.com/dynverse/dyneval)
* Comparing two predictions
* Easily visualise the trajectory, eg. using [dynplot](https://www.github.com/dynverse/dynplot)
* Extracting relevant features/genes, eg. using [dynfeature](https://www.github.com/dynverse/dynfeature)
