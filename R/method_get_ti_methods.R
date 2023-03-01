is_dynmethods_installed <- function() {
  tryCatch(
    {
      find.package("dynmethods")
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Return all TI that are installed in one or more packages
#'
#' @param method_ids The method identifiers. NULL if listing all methods
#' @param as_tibble Whether or not to return the ti_methods as a tibble
#' @param ti_packages In which packages to look for TI methods. This will by default look into dynmethods if it is installed, otherwise in dynwrap.
#' @param evaluate Whether to evaluate the functions
#'
#' @return A dataframe (or list if `as_tibble = FALSE`) containing the name (_id_) of the TI method and the function (_fun_) to load in the method.
#'
#' @keywords infer_trajectory
#'
#' @examples
#' head(get_ti_methods())
#'
#' @importFrom utils lsf.str
#' @importFrom stringr str_replace
#' @export
get_ti_methods <- function(
  method_ids = NULL,
  as_tibble = TRUE,
  ti_packages = ifelse(is_dynmethods_installed(), "dynmethods", "dynwrap"),
  evaluate = FALSE
) {
  ti_methods <- map(ti_packages, function(package) {

    if (package == "dynwrap") {
      requireNamespace("pkgload")
      root <- system.file("tests/testthat/", package = "dynwrap")
      env <- new.env()
      for (file in list.files(root, full.names = TRUE)) {
        source(file, local = env)
      }
    } else {
      requireNamespace(package)
      env <- asNamespace(package)
    }

    function_names <- lsf.str(env, pattern = "^ti_")

    map(function_names, function(function_name) {
      fun <- get(function_name, env)

      if (evaluate) {
        meth_metadata <- fun() %>% discard(is.function)
      } else {
        meth_metadata <- list(id = function_name %>% stringr::str_replace("^ti_", ""))
      }
      meth_metadata$fun <- fun
      meth_metadata
    })
  }) %>%
    unlist(recursive = FALSE) %>%
    list_as_tibble()

  if (!is.null(method_ids)) {
    assert_that(all(method_ids %in% ti_methods$id | grepl("/", method_ids)))
    ti_methods <- ti_methods %>% slice(match(method_ids, id))

    docker_repos <-
      method_ids %>%
      keep(~ grepl("/", .))

    ti_methods2 <- list_as_tibble(map(docker_repos, function(repo) {
      funner <- create_ti_method_container(repo)
      out <- funner()
      out$fun <- funner

      if (evaluate) {
        out <- out[c("id", "fun")]
      }

      out
    }))

    ti_methods <- bind_rows(ti_methods, ti_methods2)
  }

  if (as_tibble) {
    ti_methods
  } else {
    mapdf(ti_methods, identity)
  }
}

