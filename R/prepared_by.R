#' Reallocate 'Prepared by' Authorship
#'
#' Identified patterns commonly found in references such as "Prepared by", "Submitted to", "Report for", etc. that key into authorship and source, and reallocates the following source either to the publisher or author columns
#'
#' @param df a data frame or data table
#' @param x character, the name of the column containing (potential) journal names. If working through the govscienceuseR workflow, this column name is 'container'
#' @param y character, the name of the column containing author names. If working through the govscienceuseR workflow, this column name is 'author'
#' @param z character, the name of the column containing publisher names. If working through the govscienceuseR workflow, this column name is 'publisher'
#'
#' @return a data frame with changes made to the three input columns
#'
#' @examples df <- prepared_by(df, 'container', 'author', 'publisher')
#'
#' @export
#'

prepared_by <- function(df, x, y, z){
  # Just to be sure
  df <- data.frame(df)
  prep_pattern <- c("^[Pp]repared ([Bb]y|[Ff]or) [a-z]+?",
                    "^[Pp]ublished ([Bb]y|[Ii]n) [a-z]+",
                    "^[Rr]eport ([Bb]y|[Ff]or|[Tt]o) [a-z]+",
                    "^[Ss]ubmitted ([Tt]o|[Ff]or) [a-z]+")
  author_pattern <- c("(?<=[Pp]repared ([Bb]y|[Ff]or)\\s(the)?).*",
                      "(?<=[Pp]ublished ([Bb]y|[Ii]n)\\s(the)?).*",
                      "(?<=[Rr]eport ([Bb]y|[Ff]or|[Tt]o)\\s(the)?).*",
                      "(?<=[Ss]ubmitted ([Tt]o|[Ff]or)\\s(the)?).*")
  other_pattern <- c("(?<=[Pp]repared ([Bb]y|[Ff]or)\\s).*",
                     "(?<=[Pp]ublished ([Bb]y|[Ii]n)\\s).*",
                     "(?<=[Rr]eport ([Bb]y|[Ff]or|[Tt]o)\\s).*",
                     "(?<=[Ss]ubmitted ([Tt]o|[Ff]or)\\s).*")

  #Prepared by sections -- moving these over to author or publisher
  for (i in 1:nrow(df)){
    prep.by <- str_detect(df[,x[i]], prep_pattern)
    # Need to look up this anything followed by)
    df[,y[i]] <- ifelse(prep.by == T & is.na(df[,y[i]]),
                     str_extract(df[,x[i]], author_pattern),
                     df[,y[i]])
    df[,z[i]] <- ifelse(prep.by == T & !is.na(df[,y[i]]) & is.na(df[,z[i]]),
                        str_extract(df[,x[i]], other_pattern),
                        df[,z[i]])
    df[,x[i]] <- ifelse(prep.by == T,
                            str_extract(df[,x[i]], other_pattern),
                            df[,x[i]])
  }
  return(df)
}
