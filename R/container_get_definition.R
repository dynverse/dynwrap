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

  # add the remote digests
  digests <- .container_get_digests(
    image = image,
    config = config
  )

  if (!identical(digests, NA)) {
    definition$digest <- digests$digest
    definition$repo_digests <- digests$repo_digests
  }

  # return definition file
  definition
}
