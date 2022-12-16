#' Run Keras Model on Data
#'
#' Makes a prediction about the classification of the citation -- a journal, an agency, a conference, or not a citation. Classifications are designated based on 95% confidence
#'
#' @param df a data frame with the following column names: container, title, author, journal, doi,
#' @return the input data frame with five vectors appended: a column for each classification option with probabilities of classification and a fifth column with the classification for >95% probability (<95% probability for any label is marked 'unsure')
#'
#' @examples dt <- keras_classify(dt)
#'
#' @import keras
#' @export


keras_classify <- function(df){

  # REMOVE THOSE THAT ALREADY HAVE JOURNAL OR AGENCY IDENTIFIED !
  df_classify <- filter(df, !is.na(training_input))
  classify_x <- df_classify$training_input
  df_na <- filter(df, is.na(training_input))

  model <- keras::load_model_tf("~/Desktop/single_multiclass_model.tf/")

  pred <- model %>%
    predict(classify_x) %>%
    as.data.frame() %>%
    cbind(df_classify) %>%
    select(author:training_input, V1:V4) %>%
    rename("class_delete" = "V1", "class_journal" = "V2",
           "class_agency" = "V3", "class_conference" = "V4") %>%
    mutate(predict_class = case_when(
      class_journal > 0.95 ~ "journal",
      class_agency > 0.95 ~ "agency",
      class_delete > 0.95 ~ "delete",
      class_conference > 0.95 ~ "conference",
      T ~ "unsure"))

  return(pred)
}
