parse_parameter_definition <- function(parameter_definition) {
  map2(names(parameter_definition), parameter_definition, function(id, p) {
    if(p$type == "integer") {
      ParamHelpers::makeIntegerParam(id, lower=p$lower, upper=p$upper, default=p$default)
    } else if (p$type == "discrete") {
      ParamHelpers::makeDiscreteParam(id, values=p$values, default=p$default)
    } else {
      stop("invalid type")
    }
  }) %>%
    do.call(ParamHelpers::makeParamSet, .)
}
