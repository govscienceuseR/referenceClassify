#' Load pre-trained keras model
#'
#' Loads a keras model object pre-trained on references extracted from EISs
#' @return keras model
#' @export
load_keras_model <- function() {
  keras::load_model_tf(system.file("extdata/k_model", package = "referenceClassify"))
}
