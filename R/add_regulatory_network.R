
#' Add a GRN to a dynwrap object
#'
#' @inheritParams common_param
#' @param regulatory_network A data frame consisting of three columns: `"regulator"`, `"target"`, `"strength"`.
#' @param regulatory_network_sc A data frame consisting of four columns: `"cell_id"`, `"regulator"`, `"target"`, `"strength"`.
#' @param regulators The feature ids of the regulators.
#' @param targets The feature ids of the targets.
#' @param ... Extra arguments to be saved in the model.
#' 
#' @return A dynwrap object with the regulatory network added.
#'
#' @export
add_regulatory_network <- function(dataset, regulatory_network, regulatory_network_sc = NULL, regulators = NULL, targets = NULL, ...) {
  # check regulatory network
  assert_that(
    is.data.frame(regulatory_network),
    regulatory_network %has_names% c("regulator", "target", "strength"),
    is.character(regulatory_network$regulator) || is.factor(regulatory_network$regulator),
    is.character(regulatory_network$target) || is.factor(regulatory_network$target),
    is.numeric(regulatory_network$strength),
    !is.null(regulators),
    !is.null(targets),
    all(regulatory_network$regulator %in% regulators),
    all(regulatory_network$target %in% targets)
  )

  if (!is.factor(regulatory_network$regulator)) {
    regulatory_network$regulator <- factor(regulatory_network$regulator, regulators)
  }
  if (!is.factor(regulatory_network$target)) {
    regulatory_network$target <- factor(regulatory_network$target, targets)
  }

  # check sc regulatory network
  cell_ids <- dataset$cell_ids

  assert_that(
    is.data.frame(regulatory_network_sc),
    regulatory_network_sc %has_names% c("cell_id", "regulator", "target", "strength"),
    is.character(regulatory_network_sc$cell_id) || is.factor(regulatory_network_sc$cell_id),
    is.character(regulatory_network_sc$regulator) || is.factor(regulatory_network_sc$regulator),
    is.character(regulatory_network_sc$target) || is.factor(regulatory_network_sc$target),
    is.numeric(regulatory_network_sc$strength),
    !is.null(dataset$cell_ids),
    all(regulatory_network_sc$cell_id %in% dataset$cell_ids),
    all(regulatory_network_sc$regulator %in% regulators),
    all(regulatory_network_sc$target %in% targets)
  )

  if (!is.factor(regulatory_network_sc$cell_id)) {
    regulatory_network_sc$cell_id <- factor(regulatory_network_sc$cell_id, cell_ids)
  }
  if (!is.factor(regulatory_network_sc$regulator)) {
    regulatory_network_sc$regulator <- factor(regulatory_network_sc$regulator, regulators)
  }
  if (!is.factor(regulatory_network_sc$target)) {
    regulatory_network_sc$target <- factor(regulatory_network_sc$target, targets)
  }

  dataset <- dataset %>% extend_with(
    "dynwrap::with_regulatory_network",
    regulatory_network = regulatory_network,
    regulatory_network_sc = regulatory_network_sc,
    regulators = regulators,
    targets = targets,
    ...
  )

}
