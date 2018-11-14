library(tidyverse)
library(jsonlite)

devtools::load_all()

set.seed(1)

# generate the dataset
cell_ids <- paste0("cell_", seq_len(10))

n_genes <- 5
counts <- rnbinom(length(cell_ids) * n_genes, 1000, 0.99) %>%
  matrix(nrow = length(cell_ids), dimnames = list(cell_ids, paste0("gene_", seq_len(n_genes))))

expression <- log2(counts + 1)

pseudotime <- runif(length(cell_ids), 0, 10) %>% set_names(cell_ids)

group_ids <- paste0("group_", letters[1:5])
grouping <- set_names(sample(group_ids, length(cell_ids), replace = TRUE), cell_ids)

milestone_ids <- group_ids

milestone_network <- tribble(
  ~from, ~to,~directed,~length,
  group_ids[1], group_ids[2],TRUE,1,
  group_ids[2], group_ids[3],TRUE,2,
  group_ids[3], group_ids[4],TRUE,1.5,
  group_ids[3], group_ids[5],TRUE,0.5
)

progressions <- sample_n(milestone_network, length(cell_ids), replace = TRUE) %>%
  select(from, to) %>%
  mutate(
    cell_id = cell_ids,
    percentage = runif(n())
  )

milestone_percentages <- dynwrap::convert_progressions_to_milestone_percentages(cell_ids, milestone_ids, milestone_network, progressions)

divergence_regions <- tibble(
  divergence_id = "divergence_1",
  milestone_id = group_ids[3:5],
  is_start = c(TRUE, FALSE, FALSE)
)

branches <- milestone_network %>%
  mutate(branch_id = as.character(row_number())) %>%
  select(branch_id, length, directed, from, to)

branch_progressions <- progressions %>%
  left_join(
    branches %>% mutate(edge_id = as.character(row_number())),
    c("from", "to")
  ) %>%
  select(cell_id, branch_id, percentage)

branch_network <- tribble(
  ~from, ~to,
  "1", "2",
  "2", "3",
  "2", "4"
)

branches <- branches %>% select(branch_id, length, directed)

dimred <- dyndimred::dimred_pca(expression)
dimred_milestones <- dimred[sample(seq_len(nrow(dimred)), length(milestone_ids)), ]
rownames(dimred_milestones) <- milestone_ids

cell_graph <-
  bind_rows(
    tibble(from = cell_ids[1:5], to = cell_ids[2:6]),
    tibble(from = cell_ids[7:10], to = sample(cell_ids[1:6], 4))
  )

to_keep <- cell_ids[1:6]

end_state_ids <- LETTERS[1:5]
end_state_probabilities <- matrix(runif(length(cell_ids) * length(end_state_ids)), nrow = length(cell_ids))
colnames(end_state_probabilities) <- end_state_ids
end_state_probabilities <- end_state_probabilities %>% as.data.frame() %>% mutate(cell_id = cell_ids)

timings <- list("method_afterpreproc" = as.numeric(Sys.time()), "method_aftermethod" = as.numeric(Sys.time()) + 10)

# dataset with prior information
dataset <- wrap_data(
  cell_id = cell_ids
) %>%
  add_expression(
    counts,
    expression
  ) %>%
  add_prior_information(
    start_id = names(pseudotime)[which.min(pseudotime)],
    end_id = sample(cell_ids, 2),
    start_n = 1,
    end_n = 2,
    groups_id = enframe(grouping, "cell_id", "group_id"),
    groups_n = length(group_ids),
    groups_network = milestone_network %>% select(from, to),
    timecourse_continuous = (pseudotime + runif(length(pseudotime)) * 10 - 5) %>% set_names(cell_ids),
    timecourse_discrete = cut(pseudotime, breaks = 5, labels = FALSE) %>% set_names(cell_ids),
    features_id = colnames(expression)[1:2]
  )

# a model
model <- wrap_data(
  cell_id = cell_ids
) %>%
  add_linear_trajectory(
    pseudotime
  )

# save the input
.container_save_inputs(
  list2env(c(dataset, dataset$prior_information)),
  dir_input = "inst/example_inputs/text/",
  input_format = "text",
  input_ids = allowed_inputs$input_id
)

.container_save_inputs(
  list2env(c(dataset, dataset$prior_information)),
  dir_input = "inst/example_inputs/hdf5/",
  input_format = "hdf5",
  input_ids = allowed_inputs$input_id[allowed_inputs$input_id != "dataset"]
)

.container_save_inputs(
  list2env(c(dataset, dataset$prior_information)),
  dir_input = "inst/example_inputs/rds/",
  input_format = "rds",
  input_ids = allowed_inputs$input_id[allowed_inputs$input_id != "dataset"]
)

# save the output
unlink("inst/example_outputs/*", recursive=TRUE, force=TRUE)

objects <- lst(
  cell_ids = tibble(cell_ids = cell_ids),
  pseudotime = pseudotime %>% enframe("cell_id", "pseudotime"),
  group_ids,
  grouping = grouping %>% enframe("cell_id", "group_id"),
  milestone_network,
  milestone_ids,
  progressions,
  milestone_percentages,
  divergence_regions,
  branch_network,
  branches,
  branch_progressions,
  dimred = dimred %>% as.data.frame() %>% rownames_to_column("cell_id"),
  dimred_milestones = dimred_milestones %>% as.data.frame() %>% rownames_to_column("milestone_id"),
  cell_graph,
  to_keep,
  end_state_probabilities,
  timings
)

dir_output <- "inst/example_outputs/text/"
dir.create(dir_output, recursive=TRUE)
walk2(objects, names(objects), function(x, name) {
  write_text_infer(x, glue::glue("{dir_output}/{name}"))
})

# rds
dir_output <- "inst/example_outputs/rds/"
dir.create(dir_output)
write_rds(objects, file.path(dir_output, "output.rds"))

# dynwrap
dir_output <- "inst/example_outputs/dynwrap/"
dir.create(dir_output)
write_rds(model, file.path(dir_output, "output.rds"))
