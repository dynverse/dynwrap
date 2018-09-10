.container_get_definition <- function(container_id) {
  requireNamespace("babelwhale")
  lines <- babelwhale::read_file(container_id, "/code/definition.yml")

  definition <- yaml::read_yaml(text = lines)

  # add the version number
  version <- .container_get_version(container_id)

  if (!identical(version, NA)) {
    definition$version <- version
  }

  # return definition file
  definition
}
