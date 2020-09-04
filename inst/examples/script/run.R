#!/usr/bin/env Rscript

dataset <- dyncli::main()

library(dynwrap)
library(dplyr)
library(stats)
library(dyncli)

# infer trajectory
pca <- prcomp(dataset$expression)

pseudotime <- pca$x[, dataset$parameters$component]

# flip pseudotimes using start_id
if (!is.null(dataset$priors$start_id)) {
  if (mean(pseudotime[start_id]) > 0.5) {
    pseudotime <- 1 - pseudotime
  }
}

# build trajectory
trajectory <- wrap_data(cell_ids = rownames(dataset$expression)) %>%
  add_linear_trajectory(pseudotime = pseudotime)

# save output
write_output(trajectory, dataset$output)
