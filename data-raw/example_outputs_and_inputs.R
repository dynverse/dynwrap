library(tidyverse)
library(jsonlite)

devtools::load_all()

set.seed(1)

# generate the dataset
cell_ids <- paste0("cell_", seq_len(10))

n_genes <- 5
counts <- rnbinom(length(cell_ids) * n_genes, 1000, 0.99) %>%
  matrix(nrow=length(cell_ids), dimnames = list(cell_ids, paste0("gene_", seq_len(n_genes))))

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

progressions <- sample_n(milestone_network, length(cell_ids), replace=TRUE) %>%
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

dimred <- dyndimred::dimred_pca(expression)
dimred_milestones <- dimred[sample(seq_len(nrow(dimred)), length(milestone_ids)), ]
rownames(dimred_milestones) <- milestone_ids

# prior information
prior_information <- list(
  start_cells = names(pseudotime)[which.min(pseudotime)],
  end_cells = sample(cell_ids, 2),
  n_end_states = 2,
  n_start_states = 1,
  grouping_assignment = grouping,
  n_branches = length(group_ids),
  grouping_network = milestone_network %>% select(from, to),
  time = pseudotime + runif(length(pseudotime)) * 10 - 5,
  marker_features_id = colnames(expression)[1:2]
)

# save the input
counts %>% as.data.frame() %>% rownames_to_column("cell_id") %>% write_csv("inst/example_inputs/counts.csv")
expression %>% as.data.frame %>% rownames_to_column("cell_id") %>% write_csv("inst/example_inputs/expression.csv")
jsonlite::write_json(prior_information, "inst/example_inputs/prior_information.json")

# save the output
dir_output <- "inst/example_outputs/"

tibble(cell_id=cell_ids) %>% write_csv(file.path(dir_output, "cell_ids.csv"))
write_pseudotime(pseudotime, dir_output)
write_group_ids(group_ids, dir_output)
write_grouping(grouping, dir_output)
write_milestone_network(milestone_network, dir_output)
write_milestone_ids(milestone_ids, dir_output)
write_progressions(progressions, dir_output)
write_milestone_percentages(milestone_percentages, dir_output)
write_divergence_regions(divergence_regions, dir_output)
write_dimred(dimred, dir_output)
write_dimred_milestones(dimred_milestones, dir_output)
