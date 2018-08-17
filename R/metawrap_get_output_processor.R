get_output_processor <- function(output_id) {
  requireNamespace("dynwrap")
  processor <- get(paste0("add_", output_id))

  arg_classes <- formals(processor) %>% as.list() %>% map_chr(class)

  required_args <-
    arg_classes %>%
    keep(~. == "name") %>%
    names() %>%
    setdiff(c("data_wrapper", "traj", "model", "pred", "object", "trajectory", "..."))

  optional_args <-
    arg_classes %>%
    keep(~. != "name") %>%
    names()

  lst(
    processor,
    required_args,
    optional_args,
    args = c(required_args, optional_args)
  )
}
