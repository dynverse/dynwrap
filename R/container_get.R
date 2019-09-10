.container_get_definition <- function(container_id) {
  requireNamespace("babelwhale")
  lines <- babelwhale::read_file(container_id, "/code/definition.yml")
  definition_raw <- yaml::read_yaml(text = lines)

  convert_definition(definition_raw)
}

.container_get_example <- function(container_id) {
  requireNamespace("babelwhale")
  example_code <- babelwhale::read_file(container_id, "/code/example.R")

  env <- new.env()

  eval(parse(text = example_code), envir = env)

  as.list(env)
}
