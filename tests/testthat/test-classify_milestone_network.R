context("Testing classify_milestone_network")


all_networks <- list(
  "linear" = list(
    "simple" = tibble(from = "A", to = "B", length = 1, directed = TRUE),
    "intermediate" = tibble(from = c("A", "B"), to = c("B", "C"), length = 1, directed = TRUE),
    "shuffled" = tibble(from = c("A", "B", "C"), to = c("B", "C", "D"), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      tibble(from = ord[-1], to = ord[-length(ord)], length = 1, directed = TRUE)
    },
    "simple" = tibble(from = "A", to = "B", length = 1, directed = FALSE),
    "intermediate" = tibble(from = c("A", "B"), to = c("B", "C"), length = 1, directed = FALSE),
    "shuffled" = tibble(from = c("B", "D", "B"), to = c("A", "C", "C"), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      tibble(from = ord[-1], to = ord[-length(ord)], length = 1, directed = FALSE)
    }
  ),
  "cycle" = list(
    "simple" = tibble(from = c("A", "B", "C"), to = c("B", "C", "A"), length = 1, directed = TRUE),
    "shuffled" = tibble(from = c("A", "C", "B"), to = c("B", "A", "C"), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      tibble(from = ord, to = c(ord[-1], ord[[1]]), length = 1, directed = TRUE)
    },
    "simple" = tibble(from = c("A", "B", "C"), to = c("B", "C", "A"), length = 1, directed = FALSE),
    "shuffled" = tibble(from = c("A", "A", "B"), to = c("C", "B", "C"), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      tibble(from = ord, to = c(ord[-1], ord[[1]]), length = 1, directed = FALSE)
    }
  ),
  "bifurcation" = list(
    "simple" = tibble(from = c("A", "B", "B"), to = c("B", "C", "D"), length = 1, directed = TRUE),
    "intermediate" = tibble(from = c("A", "a", "B", "B", "C", "D"), to = c("a", "B", "C", "D", "c", "d"), length = 1, directed = TRUE),
    "shuffled" = tibble(from = c("B", "A", "B"), to = c("C", "B" ,"D"), length = 1, directed = TRUE),
    "small" = tibble(from = c("A", "A"), to = c("B", "C"), length = 1, directed = T),
    "long" = {
      ord <- sample(LETTERS)
      tibble(
        from = c(ord[1:4], ord[5:9], ord[c(5,11:15)]),
        to = c(ord[2:5], ord[6:10], ord[c(11,12:16)]),
        length = 1,
        directed = TRUE
      )
    },
    "simple" = tibble(from = c("A", "B", "B"), to = c("B", "C", "D"), length = 1, directed = FALSE),
    "intermediate" = tibble(from = c("A", "a", "B", "B", "C", "D"), to = c("a", "B", "C", "D", "c", "d"), length = 1, directed = FALSE),
    "shuffled" = tibble(from = c("B", "A", "D"), to = c("C", "B" ,"B"), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      tibble(
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
  "convergence" = list(
    "simple" = tibble(from = c("A", "B", "C"), to = c("C", "C", "D"), length = 1, directed = TRUE),
    "intermediate" = tibble(from = c("A", "a", "B", "b", "C", "c", "D"), to = c("a", "C", "b", "C", "c", "D", "d"), length = 1, directed = TRUE),
    "shuffled" = tibble(from = c("B", "C", "A"), to = c("C", "D" ,"C"), length = c(2, 3, 1), directed = TRUE),
    "small" = tibble(from = c("A", "B"), to = c("C", "C"), length = 1, directed = T)
  ),
  "multifurcation" = list(
    "supersimple" = tibble(from = c("B", "B", "B"), to = c("C", "D", "E"), length = 1, directed = TRUE),
    "simple" = tibble(from = c("A", "B", "B", "B"), to = c("B", "C", "D", "E"), length = 1, directed = TRUE),
    "intermediate" = tibble(from = c("A", "a", "B", "B", "B", "C", "D"), to = c("a", "B", "C", "D", "E", "c", "d"), length = 1, directed = TRUE),
    "shuffled" = tibble(from = c("B", "A", "B", "B"), to = c("C", "B" ,"D", "E"), length = 1, directed = TRUE),
    "many" = tibble(from = c("root", rep("mid", length(LETTERS))), to = c("mid", LETTERS), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      tibble(
        from = c(ord[1:4], ord[5:9], ord[c(5,11:15)], ord[c(5, 17:20)]),
        to = c(ord[2:5], ord[6:10], ord[c(11,12:16)], ord[c(17, 18:21)]),
        length = 1,
        directed = TRUE
      )
    },
    "simple" = tibble(from = c("A", "B", "B", "B"), to = c("B", "C", "D", "E"), length = 1, directed = FALSE),
    "intermediate" = tibble(from = c("A", "a", "B", "B", "C", "D", "B"), to = c("a", "B", "C", "D", "c", "d", "E"), length = 1, directed = FALSE),
    "shuffled" = tibble(from = c("B", "A", "D", "B"), to = c("C", "B" ,"B", "E"), length = 1, directed = FALSE),
    "many" = tibble(from = c("root", rep("mid", length(LETTERS))), to = c("mid", LETTERS), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      tibble(
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
  "tree" = list(
    "simple_binary" = tibble(from = c("A", "B", "B", "C", "C"), to = c("B", "C", "D", "E", "F"), length = 2, directed = TRUE),
    "intermediate" = tibble(from = c(rep("root", 6), LETTERS[1:6], LETTERS[1:6]), to = c(LETTERS[1:6], LETTERS[7:12], LETTERS[13:18]), length = 1.5, directed = TRUE),
    "shuffled" = tibble(from = c(rep("root", 6), rep("A", 10), rep("B", 3)), to = LETTERS[1:19], length = 1, directed = TRUE) %>% sample_n(nrow(.)),
    "simple_binary" = tibble(from = c("A", "B", "B", "C", "C"), to = c("B", "C", "D", "E", "F"), length = 2, directed = FALSE),
    "intermediate" = tibble(from = c(rep("root", 6), LETTERS[1:6], LETTERS[1:6]), to = c(LETTERS[1:6], LETTERS[7:12], LETTERS[13:18]), length = 1.5, directed = FALSE),
    "shuffled" = tibble(
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
  "acyclic_graph" = list(
    "bifur_conv_from_start" = tibble(from = c("A", "A", "B", "C"), to = c("B", "C", "D", "D"), length = 0.4, directed = TRUE),
    "bifur_conv" = tibble(from = c("A", "B", "B", "C", "D", "E"), to = c("B", "C", "D", "E", "E", "F"), length = 0.4, directed = TRUE),
    "directed_complete" = crossing(from = LETTERS, to = LETTERS, length = 1, directed = TRUE) %>% filter(from < to),
    "conv_bifur" = tibble(from = c("A", "B", "C", "C"), to = c("C", "C", "D", "E"), length = 1, directed = TRUE),
    "conv_bifur_big" = tibble(from = c("A", "B", "C", "D", "D", "D"), to = c("D", "D", "D", "E", "F", "G"), length = 1, directed = TRUE)
  ),
  "graph" = list(
    "simple" = tibble(from = c("A", "B", "C", "A"), to = c("B", "C", "A", "D"), length = 1.1, directed = TRUE),
    "intermediate" = tibble(from = c("A", "B", "C", sample(c("A", "B", "C"), length(letters), replace = TRUE)), to = c("B", "C", "A", sample(letters)), length = 4, directed = TRUE),
    "larger" = tibble(from = c(rep("root", 7), LETTERS[1:6], LETTERS[1:6]), to = c("root", LETTERS[1:6], LETTERS[7:12], LETTERS[13:18]), length = 1.5, directed = TRUE),
    "spiked_triangle" = tibble(from = c("A", "B", "C", "A", "B", "C"), to = c("B", "C", "A", "D", "E", "F"), length = 1, directed = TRUE)
  ),
  "disconnected_graph" = list(
    "simple" = tibble(from = c("A", "B"), to = c("C", "D"), length = 1, directed = TRUE)
  )
)

for (network_type in names(all_networks)) {
  networks <- all_networks[[network_type]]

  for (network_name in names(networks)) {
    test_that(stringr::str_glue("test whether {network_name} is detected as {network_type}"), {
      milestone_network <- networks[[network_name]]

      detected_network_type <- classify_milestone_network(milestone_network)$network_type

      expect_equal(detected_network_type, network_type)
    })
  }
}




test_that("Example trajectories match", {
  example_trajectory_types <- map(trajectory_types$example_network, mutate, directed = TRUE, length = 1) %>%
    map(classify_milestone_network) %>%
    map_chr("network_type")

  testthat::expect_equal(trajectory_types$id, example_trajectory_types)
})
