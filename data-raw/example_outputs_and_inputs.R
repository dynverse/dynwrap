library(tidyverse)

cell_ids <- paste0("cell_", seq_len(10))

n_genes <- 5
counts <- rnbinom(length(cell_ids) * n_genes, 1000, 0.99) %>%
  matrix(nrow=length(cell_ids), dimnames = list(cell_ids, paste0("gene_", seq_len(n_genes))))

expression <- log2(counts + 1)

pseudotime <- tibble(
  cell_id=cell_ids
) %>%
  mutate(pseudotime = runif(n(), 0, 10))

start_cells <- pseudotime$cell_id[which.min(pseudotime$pseudotime)]

counts %>% as.data.frame %>% write_csv("inst/example_inputs/counts.csv")
expression %>% as.data.frame %>% write_csv("inst/example_inputs/expression.csv")

start_cells %>% jsonlite::write_json("inst/example_inputs/start_cells.json")

tibble(cell_id=cell_ids) %>% write_csv("inst/example_outputs/cell_ids.csv")
pseudotime %>% write_csv("inst/example_outputs/pseudotime.csv")
