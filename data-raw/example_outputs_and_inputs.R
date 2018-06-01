library(tidyverse)
library(jsonlite)

set.seed(1)

# generate the dataset
cell_ids <- paste0("cell_", seq_len(10))

n_genes <- 5
counts <- rnbinom(length(cell_ids) * n_genes, 1000, 0.99) %>%
  matrix(nrow=length(cell_ids), dimnames = list(cell_ids, paste0("gene_", seq_len(n_genes))))

expression <- log2(counts + 1)

pseudotime <- tibble(
  cell_id=cell_ids
) %>%
  mutate(pseudotime = runif(n(), 0, 10))

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

# prior information
start_cells <- pseudotime$cell_id[which.min(pseudotime$pseudotime)]

# save the data
counts %>% as.data.frame() %>% rownames_to_column("cell_id") %>% write_csv("inst/example_inputs/counts.csv")
expression %>% as.data.frame %>% rownames_to_column("cell_id") %>% write_csv("inst/example_inputs/expression.csv")

start_cells %>% jsonlite::write_json("inst/example_inputs/start_cells.json")

tibble(cell_id=cell_ids) %>% write_csv("inst/example_outputs/cell_ids.csv")
pseudotime %>% write_csv("inst/example_outputs/pseudotime.csv")
group_ids %>% write_json("inst/example_outputs/group_ids.json")
tibble(cell_id = names(grouping), group_id = grouping) %>%
  write_csv("inst/example_outputs/grouping.csv")
milestone_network %>% write_csv("inst/example_outputs/milestone_network.csv")
milestone_ids %>% write_json("inst/example_outputs/milestone_ids.json")
progressions %>% write_csv("inst/example_outputs/progressions.csv")
milestone_percentages %>% write_csv("inst/example_outputs/milestone_percentages.csv")
divergence_regions %>% write_csv("inst/example_outputs/divergence_regions.csv")
