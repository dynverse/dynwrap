library(tidyverse)

cell_ids <- paste0("cell_", seq_len(10))

pseudotime <- tibble(
  cell_id=cell_ids
) %>%
  mutate(pseudotime = runif(n(), 0, 10))

pseudotime %>% write_csv("inst/example_outputs/pseudotime.csv")
