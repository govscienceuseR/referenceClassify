#' Match to Scimago Journal Index
#'
#' Creates a logical vector as to whether a journal name is an exact match to an index of Scimago journals
#'
#' @param x vector of potential journal names, typically the container or journal.disam column from the govscienceuseR workflow
#'
#' @return logical vector indicating an exact match for Scimago journal index
#'
#' @examples dt$journal_match <- journal_match(dt$container)
#'
#' @export

journal_match <- function(x){
  scimago.j <- fread("~/Box/truckee/data/eia_data/journal_list.csv", fill= T)
  journal_match <- ifelse(x %in% scimago.j$title, T, F)
  return(journal_match)
}
