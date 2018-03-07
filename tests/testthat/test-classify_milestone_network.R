context("milestone network classification")


all_networks <- list(
  "directed_linear" = list(
    "simple" = data_frame(from = "A", to = "B", length = 1, directed = TRUE),
    "intermediate" = data_frame(from = c("A", "B"), to = c("B", "C"), length = 1, directed = TRUE),
    "shuffled" = data_frame(from = c("A", "B", "C"), to = c("B", "C", "D"), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(from = ord[-1], to = ord[-length(ord)], length = 1, directed = TRUE)
    }
  ),
  "undirected_linear" = list(
    "simple" = data_frame(from = "A", to = "B", length = 1, directed = FALSE),
    "intermediate" = data_frame(from = c("A", "B"), to = c("B", "C"), length = 1, directed = FALSE),
    "shuffled" = data_frame(from = c("B", "D", "B"), to = c("A", "C", "C"), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(from = ord[-1], to = ord[-length(ord)], length = 1, directed = FALSE)
    }
  ),
  "directed_cycle" = list(
    "simple" = data_frame(from = c("A", "B", "C"), to = c("B", "C", "A"), length = 1, directed = TRUE),
    "shuffled" = data_frame(from = c("A", "C", "B"), to = c("B", "A", "C"), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(from = ord, to = c(ord[-1], ord[[1]]), length = 1, directed = TRUE)
    }
  ),
  "undirected_cycle" = list(
    "simple" = data_frame(from = c("A", "B", "C"), to = c("B", "C", "A"), length = 1, directed = FALSE),
    "shuffled" = data_frame(from = c("A", "A", "B"), to = c("C", "B", "C"), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(from = ord, to = c(ord[-1], ord[[1]]), length = 1, directed = FALSE)
    }
  ),
  "bifurcation" = list(
    "simple" = data_frame(from = c("A", "B", "B"), to = c("B", "C", "D"), length = 1, directed = TRUE),
    "intermediate" = data_frame(from = c("A", "a", "B", "B", "C", "D"), to = c("a", "B", "C", "D", "c", "d"), length = 1, directed = TRUE),
    "shuffled" = data_frame(from = c("B", "A", "B"), to = c("C", "B" ,"D"), length = 1, directed = TRUE),
    "small" = data_frame(from = c("A", "A"), to = c("B", "C"), length = 1, directed = T),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(
        from = c(ord[1:4], ord[5:9], ord[c(5,11:15)]),
        to = c(ord[2:5], ord[6:10], ord[c(11,12:16)]),
        length = 1,
        directed = TRUE
      )
    }
  ),
  "convergence" = list(
    "simple" = data_frame(from = c("A", "B", "C"), to = c("C", "C", "D"), length = 1, directed = TRUE),
    "intermediate" = data_frame(from = c("A", "a", "B", "b", "C", "c", "D"), to = c("a", "C", "b", "C", "c", "D", "d"), length = 1, directed = TRUE),
    "shuffled" = data_frame(from = c("B", "C", "A"), to = c("C", "D" ,"C"), length = c(2, 3, 1), directed = TRUE),
    "small" = data_frame(from = c("A", "B"), to = c("C", "C"), length = 1, directed = T)
  ),
  "simple_fork" = list(
    "simple" = data_frame(from = c("A", "B", "B"), to = c("B", "C", "D"), length = 1, directed = FALSE),
    "intermediate" = data_frame(from = c("A", "a", "B", "B", "C", "D"), to = c("a", "B", "C", "D", "c", "d"), length = 1, directed = FALSE),
    "shuffled" = data_frame(from = c("B", "A", "D"), to = c("C", "B" ,"B"), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(
        from2 = c(ord[1:4], ord[5:9], ord[c(5,11:15)]),
        to2 = c(ord[2:5], ord[6:10], ord[c(11,12:16)]),
        length = 1,
        directed = FALSE
      ) %>%
        mutate(
          mix = sample(c(T, F), n(), replace = TRUE),
          from = ifelse(mix, from2, to2),
          to = ifelse(mix, to2, from2)
        ) %>%
        select(from, to, length, directed)
    }
  ),
  "multifurcation" = list(
    "simple" = data_frame(from = c("A", "B", "B", "B"), to = c("B", "C", "D", "E"), length = 1, directed = TRUE),
    "intermediate" = data_frame(from = c("A", "a", "B", "B", "B", "C", "D"), to = c("a", "B", "C", "D", "E", "c", "d"), length = 1, directed = TRUE),
    "shuffled" = data_frame(from = c("B", "A", "B", "B"), to = c("C", "B" ,"D", "E"), length = 1, directed = TRUE),
    "many" = data_frame(from = c("root", rep("mid", length(LETTERS))), to = c("mid", LETTERS), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(
        from = c(ord[1:4], ord[5:9], ord[c(5,11:15)], ord[c(5, 17:20)]),
        to = c(ord[2:5], ord[6:10], ord[c(11,12:16)], ord[c(17, 18:21)]),
        length = 1,
        directed = TRUE
      )
    }
  ),
  "complex_fork" = list(
    "simple" = data_frame(from = c("A", "B", "B", "B"), to = c("B", "C", "D", "E"), length = 1, directed = FALSE),
    "intermediate" = data_frame(from = c("A", "a", "B", "B", "C", "D", "B"), to = c("a", "B", "C", "D", "c", "d", "E"), length = 1, directed = FALSE),
    "shuffled" = data_frame(from = c("B", "A", "D", "B"), to = c("C", "B" ,"B", "E"), length = 1, directed = FALSE),
    "many" = data_frame(from = c("root", rep("mid", length(LETTERS))), to = c("mid", LETTERS), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(
        from2 = c(ord[1:4], ord[5:9], ord[c(5,11:15)], ord[c(5, 17:20)]),
        to2 = c(ord[2:5], ord[6:10], ord[c(11,12:16)], ord[c(17, 18:21)]),
        length = 1,
        directed = FALSE
      ) %>%
        mutate(
          mix = sample(c(T, F), n(), replace = TRUE),
          from = ifelse(mix, from2, to2),
          to = ifelse(mix, to2, from2)
        ) %>%
        sample_n(nrow(.)) %>%
        select(from, to, length, directed)
    }
  ),
  "rooted_tree" = list(
    "simple_binary" = data_frame(from = c("A", "B", "B", "C", "C"), to = c("B", "C", "D", "E", "F"), length = 2, directed = TRUE),
    "intermediate" = data_frame(from = c(rep("root", 6), LETTERS[1:6], LETTERS[1:6]), to = c(LETTERS[1:6], LETTERS[7:12], LETTERS[13:18]), length = 1.5, directed = TRUE),
    "shuffled" = data_frame(from = c(rep("root", 6), rep("A", 10), rep("B", 3)), to = LETTERS[1:19], length = 1, directed = TRUE) %>% sample_n(nrow(.))
  ),
  "unrooted_tree" = list(
    "simple_binary" = data_frame(from = c("A", "B", "B", "C", "C"), to = c("B", "C", "D", "E", "F"), length = 2, directed = FALSE),
    "intermediate" = data_frame(from = c(rep("root", 6), LETTERS[1:6], LETTERS[1:6]), to = c(LETTERS[1:6], LETTERS[7:12], LETTERS[13:18]), length = 1.5, directed = FALSE),
    "shuffled" = data_frame(
      from2 = c(rep("root", 6), rep("A", 10), rep("B", 3)),
      to2 = LETTERS[1:19], length = 1, directed = FALSE
    ) %>%
      mutate(
        mix = sample(c(T, F), n(), replace = TRUE),
        from = ifelse(mix, from2, to2),
        to = ifelse(mix, to2, from2)
      ) %>%
      sample_n(nrow(.)) %>%
      select(from, to, length, directed)
  ),
  "directed_acyclic_graph" = list(
    "bifur_conv_from_start" = data_frame(from = c("A", "A", "B", "C"), to = c("B", "C", "D", "D"), length=0.4, directed=TRUE),
    "bifur_conv" = data_frame(from = c("A", "B", "B", "C", "D", "E"), to = c("B", "C", "D", "E", "E", "F"), length = 0.4, directed = TRUE),
    "directed_complete" = crossing(from = LETTERS, to = LETTERS, length = 1, directed = TRUE) %>% filter(from < to)
  ),
  "directed_graph" = list(
    "simple" = data_frame(from = c("A", "B", "C", "A"), to = c("B", "C", "A", "D"), length = 1.1, directed = TRUE),
    "intermediate" = data_frame(from = c("A", "B", "C", LETTERS), to = c("B", "C", "A", sample(LETTERS)), length = 4, directed = TRUE),
    "larger" = data_frame(from = c(rep("root", 7), LETTERS[1:6], LETTERS[1:6]), to = c("root", LETTERS[1:6], LETTERS[7:12], LETTERS[13:18]), length = 1.5, directed = TRUE),
    "spiked_triangle" = data_frame(from = c("A", "B", "C", "A", "B", "C"), to = c("B", "C", "A", "D", "E", "F"), length = 1, directed = TRUE)
  ),
  "undirected_graph" = list(
    "bifur_conv_undirected" = data_frame(from = c("A", "B", "B", "C", "D", "E"), to = c("B", "C", "D", "E", "E", "F"), length = 0.4, directed = FALSE),
    "complete" = crossing(from = LETTERS, to = LETTERS, length = 1, directed = FALSE) %>% filter(from < to),
    "simple" = data_frame(from = c("A", "B", "C", "A"), to = c("B", "C", "A", "D"), length = 1.1, directed = FALSE),
    "intermediate" = data_frame(from = c("A", "B", "C", LETTERS), to = c("B", "C", "A", sample(LETTERS)), length = 4, directed = FALSE),
    "larger" = data_frame(from = c(rep("root", 7), LETTERS[1:6], LETTERS[1:6]), to = c("root", LETTERS[1:6], LETTERS[7:12], LETTERS[13:18]), length = 1.5, directed = FALSE),
    "spiked_triangle" = data_frame(from = c("A", "B", "C", "A", "B", "C"), to = c("B", "C", "A", "D", "E", "F"), length = 1, directed = FALSE)
  )
)

for (network_type in names(all_networks)) {
  networks <- all_networks[[network_type]]

  for (network_name in names(networks)) {
    test_that(pritt("test whether {network_name} is detected as {network_type}"), {
      cat(pritt("test whether {network_name} is detected as {network_type}"), "\n", sep = "")
      network <- networks[[network_name]]

      detected_network_type <- classify_milestone_network(network)$network_type

      expect_equal(detected_network_type, network_type)
    })
  }
}

# all_nets <- map_df(names(all_networks), function(network_type) {
#   network_names <- names(all_networks[[network_type]])
#   map_df(network_names, function(network_name) {
#     tibble(
#       network_type,
#       network_name,
#       milestone_network = list(all_networks[[network_type]][[network_name]])
#     )
#   })
# }) %>%
#   rowwise() %>%
#   mutate(
#     pl2 = list({
#       cat(network_name, " -- ", network_type, "\n", sep = "")
#       cell_ids <- paste0("Cell", seq_len(nrow(milestone_network)))
#       data <- wrap_data(
#         "one",
#         paste0(network_name, " is a ", network_type),
#         cell_ids = cell_ids,
#         milestone_ids = unique(c(milestone_network$from, milestone_network$to)),
#         milestone_network = milestone_network,
#         progressions = data_frame(cell_id = cell_ids, from = milestone_network$from, to = milestone_network$to, percentage = .5))
#       dynplot::plot_default(data)
#     })
#   ) %>%
#   ungroup()
# cowplot::plot_grid(plotlist = all_nets$pl2)
#
# pdf("~/testplots.pdf", 6, 6)
# for (pl in all_nets$pl2)
#   print(pl)
# dev.off()
