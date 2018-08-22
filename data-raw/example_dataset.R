library(tidyverse)
library(dynwrap)

set.seed(1)

ncells <- 1000
pseudotime <- runif(ncells)

expression <- matrix(
  c(
    (pseudotime - 0.5) ** 2,
    sqrt(pseudotime + 20),
    pseudotime
  ),
  ncol = 3,
  dimnames = list(as.character(rep(seq_len(ncells))), as.character(c("A", "B", "C")))
)
expression <- expression + rnorm(length(expression), sd = 0.02)

names(pseudotime) <- rownames(expression) <- paste0("Cell", seq_len(nrow(expression)))

start_id <- rownames(expression)[which.min(pseudotime)]

counts <- round(expression)

example_dataset <-
  wrap_data(
    id = "example",
    cell_ids = rownames(counts)
  ) %>%
  add_expression(
    expression = expression,
    counts = counts
  ) %>%
  add_linear_trajectory(
    pseudotime = pseudotime,
    directed = TRUE
  ) %>%
  add_dimred(
    dimred = expression
  ) %>%
  add_prior_information() %>%
  add_cell_waypoints()

dynplot::plot_graph(example_dataset)
dynplot::plot_dimred(example_dataset)
dynplot::plot_heatmap(example_dataset)

devtools::use_data(example_dataset, overwrite = TRUE)
