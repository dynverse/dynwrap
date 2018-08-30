.container_get_definition <- function(
  image,
  config = container_config()
) {
  out <- .container_run(
    image = image,
    command = "cat",
    extra_args = "/code/definition.yml",
    config = config,
    verbose = FALSE
  )

  definition <- yaml::read_yaml(text = sub("^.*\n(id: [^\n]*\n.*)", "\\1", out$stdout))

  # add the version number
  version <- .container_get_version(image, config)

  if (!identical(version, NA)) {
    definition$version <- version
  }

  # return definition file
  definition
}
