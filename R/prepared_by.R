#' Reallocate 'Prepared by' Authorship
#'
#' Identifies patterns commonly found in references such as "Prepared by", "Submitted to", "Report for", etc. that key into authorship and source, and reallocates the following source either to the publisher or author columns
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

#Retrieved from
prepared_by <- function(df, x, y, z){
  # Just to be sure
  df <- data.frame(df)
  # A simple function
  rpl_na <- function(x){
    ifelse(x == "", NA, x)
  }
  # I would typically do this with a df
  df <- data.frame(sapply(df, rpl_na))

  prep_pattern <- paste(c("^[Pp]repared ([Bb]y|[Ff]or) [a-z+]?",
                    "^[Pp]ublished ([Bb]y|[Ii]n) [a-z+]?",
                    "^[Rr]eport ([Bb]y|[Ff]or|[Tt]o) [a-z+]?",
                    "^[Ss]ubmitted ([Tt]o|[Ff]or) [a-z+]?"), collapse = "|")
  author_pattern <- paste(c("(?<=[Pp]repared ([Bb]y|[Ff]or)\\s(the)?).*",
                      "(?<=[Pp]ublished ([Bb]y|[Ii]n)\\s(the)?).*",
                      "(?<=[Rr]eport ([Bb]y|[Ff]or|[Tt]o)\\s(the)?).*",
                      "(?<=[Ss]ubmitted ([Tt]o|[Ff]or)\\s(the)?).*"), collapse = "|")
  other_pattern <- paste(c("(?<=[Pp]repared ([Bb]y|[Ff]or)\\s).*",
                     "(?<=[Pp]ublished ([Bb]y|[Ii]n)\\s).*",
                     "(?<=[Rr]eport ([Bb]y|[Ff]or|[Tt]o)\\s).*",
                     "(?<=[Ss]ubmitted ([Tt]o|[Ff]or)\\s).*"), collapse = "|")

  #Prepared by sections -- moving these over to author or publisher
  for (i in 1:nrow(df)){
    prep.by <- str_detect(df[i,x], prep_pattern)
    # Need to look up this anything followed by)
    df[i,y] <- ifelse(prep.by == T & is.na(df[i,y]),
                     str_extract(df[i,x], author_pattern),
                     df[i,y])
    df[i,z] <- ifelse(prep.by == T & !is.na(df[i,y]) & is.na(df[i,z]),
                        str_extract(df[i,x], other_pattern),
                        df[i,z])
    df[i,x] <- ifelse(prep.by == T,
                            str_extract(df[i,x], other_pattern),
                            df[i,x])
  }
  return(data.table(df))
}
