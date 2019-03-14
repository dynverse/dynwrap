.container_get_definition <- function(container_id) {
  requireNamespace("babelwhale")
  lines <- babelwhale::read_file(container_id, "/code/definition.yml")
  definition_raw <- yaml::read_yaml(text = lines)

  .method_convert_definition(definition_raw)
}
