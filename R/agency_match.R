#' Match to Agency Index
#'
#' Creates a logical vector as to whether an author name or publication name is an exact match to an index of government agencies
#'
#' @param x a vector of potential agency names, typically the container or journal.disam column from the govscienceuseR workflow
#' @param y a econd vector of potential agency namess, typically the author column from the govscienceuseR workflow
#' @param append T or F argument to whether user wants to add their own list of agencies to be indexed. Default append = F
#' @param append_df a data frame with three character columns: State (level of government), Agency (name of agency), and Abbr (abbreviation of agency) to append to the built-in index
#'
#' @return logical vector indicating an exact match to the agency index
#'
#' @examples dt$agency_match <- agency_match(dt$container)
#'
#' @export

agency_match <- function(x, y, append = F, append_df){
  agencies <- fread("~/Box/truckee/data/eia_data/agency_list.csv")
  if(append == T){
    agencies <- rbind(agencies, append_df)
  }
    agency_match <- ifelse(x %in% agencies$Agency | y %in% agencies$Agency, T, F)
    return(agency_match)
}
