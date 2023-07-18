#' Add a feature importance to a dataset
#'
#' @inheritParams common_param
#' @param feature_importance The impotances of the features, can be a named vector or a dataframe with columns *feature_id* and *importance*
#' @param ... Extra information to be stored in the dataset
#'
#' @keywords adapt_trajectory
#' 
#' @return A dynwrap object with the feature importance added.
#'
#' @examples
#' dataset <- example_dataset
#'
#' feature_importance <- runif(nrow(dataset$feature_info))
#' names(feature_importance) <- dataset$feature_info$feature_id
#'
#' dataset <- add_feature_importance(dataset, feature_importance)
#' head(dataset$feature_importance)
#'
#' @export
add_feature_importance <- function(
  dataset,
  feature_importance,
  ...
) {
  if (is.vector(feature_importance)) {
    assert_that(!is.null(names(feature_importance)))

    feature_importance <- enframe(feature_importance, "feature_id", "importance")
  }

  assert_that(
    # check whether dataset is a data wrapper
    is_data_wrapper(dataset),

    is.data.frame(feature_importance),
    "feature_id" %in% colnames(feature_importance),
    "importance" %in% colnames(feature_importance)
  )

  # create output structure
  dataset %>% extend_with(
    "dynwrap::with_feature_importance",
    feature_importance = feature_importance,
    ...
  )
}

#' @rdname add_feature_importance
#' @export
is_wrapper_with_feature_importance <- function(dataset) {
  is_data_wrapper(dataset) && "dynwrap::with_feature_importance" %in% class(dataset)
}




