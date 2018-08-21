.container_get_singularity_images_folder <- function(image_type) {
  if (!identical(image_type, "singularity")) {
    return(invisible())
  }

  if (Sys.getenv("DYNWRAP_SINGULARITY_IMAGES_FOLDER") != "") {
    Sys.getenv("DYNWRAP_SINGULARITY_IMAGES_FOLDER")
  } else if (!is.null(getOption("dynwrap_singularity_images_folder"))) {
    getOption("dynwrap_singularity_images_folder")
  } else {
    warning(
      "No 'singularity_image_folder' specified, 'dynwrap_singularity_images_folder' option not set ",
      "and 'DYNWRAP_SINGULARITY_IMAGES_FOLDER' environment variable not set. ",
      "Putting images in working directory."
    )
    "./"
  }
}
