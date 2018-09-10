.container_get_example <- function(container_id) {
  requireNamespace("babelwhale")
  example_code <- babelwhale::read_file(container_id, "/code/example.R")

  env <- new.env()

  eval(parse(text = example_code), envir = env)

  as.list(env)
}
