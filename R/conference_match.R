#' Match to Scimago Conference Index
#'
#' Creates a logical vector as to whether a journal name is an exact match to an index of Scimago conferences
#'
#' @param x vector of potential conference names, typically the container or journal.disam column from the govscienceuseR workflow
#'
#' @return logical vector indicating an exact match for Scimago conference index
#'
#' @examples dt$conference_match <- conference_match(dt$container)
#'
#' @export

conference_match <- function(x){
  #scimago.c <- data.table(readRDS("data/conference_list.RDS"))
  #usethis::use_data(scimago.c)
  data("scimago.c", envir=environment())
  conference_match <- ifelse(x %in% scimago.c$title, T, F)
  return(conference_match)
}
