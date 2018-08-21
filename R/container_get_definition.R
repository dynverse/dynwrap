.container_get_definition <- function(
  image,
  image_type,
  singularity_images_folder = .container_get_singularity_images_folder(image_type)
) {
  tempfile <- safe_tempdir("tmp_mount")
  definition_location <- "/code/definition.yml"

  if (image_type == "docker") {
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
  } else if (image_type == "singularity") {
    image_location <- normalizePath(paste0(singularity_images_folder, "/", image, ".simg"), mustWork = FALSE)

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
  definition$remote_digests <-
    .container_get_remote_digests(
      image = image,
      image_type = image_type,
      singularity_images_folder = singularity_images_folder
    )

  # return definition file
  definition
}
