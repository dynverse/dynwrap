.container_get_example <- function(image) {
  example_code <- .container_read_file(image = image, path_container = "/code/example.R")

  env <- new.env()
  eval(parse(text = example_code), envir = env)

  as.list(env)
}
