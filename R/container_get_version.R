#' @importFrom babelwhale get_label
.container_get_version <- function(container_id) {
  babelwhale::get_label(container_id, "version")
}
