#' Parse a parameter definition
#'
#' @param parameter_definition A named list containing the parameters. Each list has at least a type (integer, numeric, interger_vector, numeric_vector, discrete, discrete_vector, logical, logical_vector) and a default value. Other properties specify the parameter space (distribution, lower, upper, mean, sd, rate). A parameter forbidden can also be specified.
#' @export
parse_parameter_definition <- function(parameter_definition) {
  if(!is.null(parameter_definition$forbidden)) {
    forbidden <- parse(text = parameter_definition$forbidden)
    parameter_definition <- parameter_definition[names(parameter_definition) != "forbidden"]
  } else {
    forbidden <- NULL
  }

  map2(names(parameter_definition), parameter_definition, function(id, param) {
    if(!all(c("type", "default") %in% names(param))) {
      stop("Parameter should at least define type and default")
    }

    if (is.null(param$special_values)) param$special_values <- list()
    if (is.null(param$tunable)) param$tunable <- TRUE

    if(param$type %in% c("integer", "numeric", "integer_vector", "numeric_vector")) {
      if (is.null(param$lower)) param$lower <- param$default
      if (is.null(param$upper)) param$upper <- param$default
      if (is.null(param$distribution)) param$distribution <- "uniform"
      distribution2uniform <- get_distribution2uniform(param)
      uniform2distribution <- get_uniform2distribution(param)

      if (param$type %in% c("integer", "numeric")) {
        ParamHelpers::makeNumericParam(
          id,
          lower = param$lower %>% distribution2uniform,
          upper = param$upper %>% distribution2uniform,
          default = param$default %>% distribution2uniform,
          trafo = uniform2distribution,
          special.vals = param$special_values,
          tunable = param$tunable
        )
      } else {
        ParamHelpers::makeNumericVectorParam(
          id,
          lower = param$lower %>% distribution2uniform,
          upper = param$upper %>% distribution2uniform,
          default = param$default %>% distribution2uniform,
          trafo = uniform2distribution,
          len = param$length,
          special.vals = param$special_values,
          tunable = param$tunable
        )
      }
    } else if (param$type == "discrete") {
      if (is.null(param$values)) {param$values <- param$default}
      ParamHelpers::makeDiscreteParam(
        id,
        values = param$values,
        default = param$default,
        special.vals = param$special_values,
        tunable = param$tunable
      )
    } else if (param$type == "discrete_vector") {
      ParamHelpers::makeDiscreteVectorParam(
        id,
        values = as.list(param$values),
        default = as.list(param$default),
        len = param$length,
        special.vals = param$special_values,
        tunable = param$tunable
      )
    } else if (param$type == "logical") {
      ParamHelpers::makeLogicalParam(
        id,
        default = param$default,
        special.vals = param$special_values,
        tunable = param$tunable
      )
    } else if (param$type == "logical_vector") {
      ParamHelpers::makeLogicalVectorParam(
        id,
        default = param$default,
        len = param$length,
        special.vals = param$special_values,
        tunable = param$tunable
      )
    } else {
      stop("invalid type")
    }
  }) %>%
    invoke(ParamHelpers::makeParamSet, ., forbidden = forbidden)
}



#' @importFrom stats pnorm pexp punif
get_distribution2uniform <- function(param) {
  switch(
    param$distribution,
    normal = {
      if(is.null(param$mean)) {stop("Provide mean when using a normal distributed parameter")}
      if(is.null(param$sd)) {stop("Provide sd when using a normal distributed parameter")}
      function(q) stats::pnorm(q, mean = param$mean, sd = param$sd)
    },
    exponential = {
      if(is.null(param$rate)) {stop("Provide rate when using a normal distributed parameter")}
      if(is.null(param$upper)) {stop("Provide upper when using a normal distributed parameter")}

      function(q) stats::pexp(q, rate = param$rate)
    },
    uniform = {
      if(param$lower == -Inf) {stop("Provide lower boundary when using an uniformly distributed parameter")}
      if(param$upper == -Inf) {stop("Provide upper boundary when using an uniformly distributed parameter")}
      function(q) stats::punif(q, param$lower, param$upper)
    }
  )
}

#' @importFrom stats qnorm qexp qunif
get_uniform2distribution <- function(param) {
  uniform2distribution <- switch(
    param$distribution,
    normal = function(p) stats::qnorm(p, mean = param$mean, sd = param$sd),
    exponential = function(p) stats::qexp(p, rate = param$rate),
    uniform = function(p) stats::qunif(p, param$lower, param$upper)
  )

  if (param$type %in% c("integer", "integer_vector")) {
    function(x) round(uniform2distribution(x))
  } else {
    uniform2distribution
  }
}
