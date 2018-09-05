.container_copy_file <- function(
  image,
  config = container_config(),
  path_container,
  path_local
) {

  if (config$type == "docker") {
    # start container
    output <- processx::run(
      "docker",
      c("create", "--entrypoint", "bash", image),
      stderr_callback = print_processx
    )
    id <- trimws(utils::tail(output$stdout, 1))

    # copy file from container
    processx::run(
      "docker",
      c("cp", paste0(id, ":", path_container), path_local),
      stderr_callback = print_processx
    )

    # remove container
    processx::run("docker", c("rm", id), stderr_callback = print_processx)

    invisible()
  } else if (config$type == "singularity") {
    temp_folder <- safe_tempdir("")
    on.exit(unlink(temp_folder, recursive = TRUE, force = TRUE))

    .container_run(
      image = image,
      command = "cp",
      extra_args = c(path_container, "/copy_mount"),
      volumes = paste0(temp_folder, ":/copy_mount"),
      config = config
    )

    file.copy(paste0(temp_folder, gsub(".*/", "", path_container)), path_local)
  }
}
