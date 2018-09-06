.container_get_definition <- function(image) {
  lines <- .container_read_file(image = image, path_container = "/code/definition.yml")

  definition <- yaml::read_yaml(text = lines)

  # add the version number
  version <- .container_get_version(image)

  if (!identical(version, NA)) {
    definition$version <- version
  }

  # return definition file
  definition
}
