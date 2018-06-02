library(tibble)

priors <- dynwrap:::priors

prior_usages <- tribble(
  ~prior_usage, ~color,
  "optional", "#0074D9",
  "no", "#EEEEEE",
  "required", "#FF4136"
)

devtools::use_data(priors, prior_usages, overwrite = TRUE)
