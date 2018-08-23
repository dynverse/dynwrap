.container_get_definition <- function(
  image,
  config = container_config()
) {
  tempfile <- safe_tempdir("tmp_mount")
  definition_location <- "/code/definition.yml"

  if (config$type == "docker") {
    # start container
    output <- processx::run(
      "docker",
      c("create", "--entrypoint", "bash", image),
      stderr_callback = print_processx
    )
    id <- trimws(utils::tail(output$stdout, 1))

    # remove container
    on.exit(processx::run("docker", c("rm", id), stderr_callback = print_processx))

    # copy file from container
    processx::run(
      "docker",
      c("cp", glue::glue("{id}:{definition_location}"), paste0(tempfile, "/definition.yml")),
      stderr_callback = print_processx
    )
  } else if (config$type == "singularity") {
    image_name <- gsub("[:@].*$", "", image)
    image_location <- normalizePath(paste0(config$images_folder, "/", image_name, ".simg"), mustWork = FALSE)

    processx::run(
      "singularity",
      c(
        "exec",
        "-B",
        glue("{tempfile}:/tmp_folder"),
        image_location,
        "cp", definition_location, "/tmp_folder"
      ),
      stderr_callback = print_processx
    )
  }

  # read definition file
  definition <- yaml::read_yaml(paste0(tempfile, "/definition.yml"))

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
