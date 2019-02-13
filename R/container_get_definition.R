.container_get_definition <- function(container_id) {
  requireNamespace("babelwhale")
  lines <- babelwhale::read_file(container_id, "/code/definition.yml")
  yaml::read_yaml(text = lines)
}
