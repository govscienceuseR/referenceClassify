#' Classification With Regex
#'
#' Matches citations to journals, agencies, conferences, and patterns of non-matches (e.g. dates, photos, etc.) to classify them into one of those four groups.
#'
#' @param df a data frame or data table output from the journal_disambig() function (has columns:)
#' @param journal_column the name
#'
#' @return a data frame with two new columns: training_input and class
#'
#' @examples df <- regex_classify(df, 'journal_disam')
#'
#' @import magrittr
#' @import stringr
#' @export
#'

regex_classify <- function(df, journal_column){
  data("scimago.j", envir=environment())
  data("agencies", envir=environment())
  data("scimago.c", envir=environment())
  agency.pattern <- paste(agencies$Agency, collapse = "\\b|\\b")

  # These should have already been removed in the citation_clean function, but just in case
  rm_word <- c( '^[a-z]\\.\\s', # Many authors begind with a. b. or c. etc as if its a list.
                "^,\\s", # Dealing with if NAs were in the start
                "^,$", # If it pasted 2 empties
                "_{2,6}", # If there are multiple underscores
                "^\\/\\s?", #Revmoe the forward slash and space to start
                "^[:punct:]+$", # only punctuation
                "[~><■✔►]+", # only punctuation
                "^:\\s",
                "[Aa] [Rr]eport by (the )?",
                "[Aa] [Rr]eport for (the )?",
                "[Aa] [Rr]eport of (the )?",
                "[Aa] [Rr]eport to (the )?",
                "[Aa] [Rr]eport prepared by (the )?",
                "Accessed,?",
                "^Bull$|^Bulletin$",
                "Internet [Ww]ebsite",
                "^Fiscal [Yy]ears?$",
                "^Final [Dd]ecision",
                "Prepared by",
                "Prepared for (the )?",
                "Final [Rr]eport to (the )?",
                "[Jj][Aa][Nn][Uu][Aa][Rr][Yy],?\\s?",
                "[Ff][Ee][Bb][Rr][Uu][Aa][Rr][Yy],?\\s?",
                "[Mm][Aa][Rr][Cc][Hh],?\\s?", "[Aa][Pp][Rr][Ii][Ll],?\\s?",
                "[Mm][Aa][Yy],?\\s?", "[Jj][Uu][Nn][Ee],?\\s?",
                "[Jj][Uu][Ll][Yy],?\\s?",
                "[Aa][Uu][Gg][Uu][Ss][Tt],?\\s?",
                "[Ss][Ee][Pp][Tt][Ee][Mm][Bb][Ee][Rr],?\\s?",
                "[Oo][Cc][Tt][Oo][Bb][Ee][Rr],?\\s?",
                "[Nn][Oo][Vv][Ee][Mm][Bb][Ee][Rr],?\\s?",
                "[Dd][Ee][Cc][Ee][Mm][Bb][Ee][Rr],?\\s?")
  rm_word <- paste(rm_word, collapse="|")

  df <- df %>%
    mutate_all(na_if,"") %>%
    mutate(container = str_remove_all(container, rm_word),
           publisher = str_remove_all(publisher, rm_word),
           title = str_remove_all(title, rm_word))

  df$container_match_journal <- journal_match(df[[journal_column]])
  df$pub_match_journal <- journal_match(df$publisher)
  df$container_match_agency <- agency_match(df[[journal_column]], df[[journal_column]])
  df$author_match_agency <- agency_match(df$author, df$author)
  df$pub_match_agency <- agency_match(df$publisher, df$publisher)
  df$container_match_conf <- conference_match(df[[journal_column]])
  df$pub_match_conf <- conference_match(df$publisher)
  df$container_match_agency_p <- str_detect(df[[journal_column]], agency.pattern)
  df$author_match_agency_p <- str_detect(df$author, agency.pattern)
  df$pub_match_agency_p <- str_detect(df$publisher, agency.pattern)

df <- df %>%
    mutate(training_input = case_when(
      container_match_journal == T ~ container,
      pub_match_journal == T ~ publisher,
      author_match_agency == T ~ author, # choosing this because in cases where both match, Federal Reg is more likely to be the journal and the more specific name in the author line
      container_match_agency == T ~ container,
      pub_match_agency == T ~ publisher,
      container_match_conf == T ~ container,
      pub_match_conf == T ~ publisher,
      container_match_agency_p ~ container,
      author_match_agency_p ~ author,
      pub_match_agency_p ~ publisher,
      !is.na(doi) ~ doi,
      !is.na(container) ~ container,
      !is.na(publisher) ~ publisher,
      !is.na(author) ~ author,
      !is.na(title) ~ title,
      T ~ NA_character_
    )) %>%
  # Get rid of data that has nothing to train on
    filter(!is.na(training_input))

  rm_row <- c( "^[0-9]+$", # only digits
               "^_\\d", # Starting with an underscore then number indicates it is probably a file
               "^_[A-Za-z]",
               "^_{2,}",
               "^\\d{1,2}\\:\\d{2}", # time
               "\\d{1,2}\\:\\d{2}\\s?[Aa].?[Mm].?", # time am
               "\\d{1,2}\\:\\d{2}\\s?[Pp].?[Mm].?", # time pm
               "^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}",
               "\\bP\\.?O\\.? Box", # address
               "Box, P\\.?O\\.?", # address
               "^Address",
               "Ave\\s@\\s|Rd\\s@\\s|Dr\\s@\\s|Blvd\\s@\\s|Ln\\s@\\s", #address
               "\\d{1,2}\\syears\\sof\\sexperience", #resume
               "^Experience\\:", # resume
               "\\bB\\.?Sc\\.?\\b",
               "\\bP\\.?[Hh]\\.?[Dd]\\.?", # Various honorariums
               "^[[:punct:]]$", #punctuation only
               "^Contact\\sfor\\sMore", # Emails
               "^Attachment$",
               '^C:\\\\', # website of sorts
               #"<?https?",
               "^Date$",
               "^Ibid$",
               "^Note[sd]?$",
               "^[Nn]otes, [Mm]eeting$",
               "^[Nn]ote[sd]?, [Cc]omment$",
               "^Comments?$",
               "^Regulations?$",
               "^Rehabilitation$",
               "^[Mm][Oo][Nn][Dd][Aa][Yy],?\\s?",
               "^[Tt][Uu][Ee][Ss][Dd][Aa][Yy],?\\s?",
               "^[Ww][Ee][Dd][Nn][Ee][Ss][Dd][Aa][Yy],?\\s?",
               "^[Tt][Hh][Uu][Rr][Ss][Dd][Aa][Yy],?\\s?",
               "^[Ff][Rr][Ii][Dd][Aa][Yy],?\\s?",
               "^[Ss][Aa][Tt][Uu][Rr][Dd][Aa][Yy],?\\s?",
               "^[Ss][Uu][Nn][Dd][Aa][Yy],?\\s?",
               # As if these were sentences or resolutions; cleaned from EIA data
               "^ACTION", "^Accession$", "^Additionally",
               "^Also", "^AM$", "^Avenue$", "^BE IT FURTHER RESOLVED",
               "PUBLIC COMMENT MEETING", "^Rd$", "Responses to comment",
               "^Assembly Bill$", "^BEFORE THE", "^Attachment$", "^E\\-?mail",
               "^Email [Cc]ommunication$", "^Executive Director", "^Experience$",
               "^Express$", "^Expwy$", "^Fax$", "^FAX", "^FROM\\:",
               "^Further Information", "^Given the adequacy", "^Homepage$",
               "^However", "^I‐\\d\\d", "^I\\‐\\d\\d", "^I\\d\\d", "^In addition",
               "^In compliance", "^In other words",  "^Informational Meeting",
               "^In preparation", "^In press", "In progress", "In submission",
               "^Last Accessed", "^Last [Aa]mended", "^Last [Mm]odified",
               "^Last [Rr]eviewed", "^Last [Rr]evised", "^Last [Uu]pdated",
               "^Location$", "^NOTICE|^Notice", "^[Pp]ersonal [Cc]ommunication",
               "^[Pp]ersonal [Ii]nterview", "^Phone$",  "^Please", "^Photo",
               "^Image", "^Public [Mm]eeting",  "^Recieved$",  "^Release$",
               "^Response$", "^Responses to Comment$",  "^Retrieved$", "^Review$",
               "^Reporting Form$", "^Rept$", "^Research$", "^Resolution$",
               "^Review$", "^Revised", "^Revision$", "^Review Period$",  "^Rule$",
               "^St$",  "^SUBJECT\\:|^Subject\\:",  "^Senate [Bb]ill$",
               "^South$",  "^Study$",  "^Tel$", "^Telephone$",  "^The$",
               "^Therefore", "^These", "^This", "^Thus", "^To\\s",
               "^Wkdy$",  "^WHEREAS")
  rm_row <- paste(rm_row, collapse="|")

  conference <- paste(c("[Cc]onference(?!\\sCenter)", "[Cc]onference(?!\\sHall)",
                        "[Ss]ymposium"), collapse = "|")

  df <- df %>%
    mutate(class = case_when(
      #  Remove if it is in the removal patterns
      str_detect(training_input, rm_row) ~ "remove_row",
      # Remove if it is very short or long
      nchar(training_input) > 250 | nchar(training_input) < 3 ~ "remove_row",
      # Assign journal name if exact match
      container_match_journal == T | pub_match_journal == T ~ "journal",
      # Assign agency if exact match
      author_match_agency_p == T | container_match_agency_p == T |
        pub_match_agency_p == T ~ "agency",
      # Assign conference if exact match
      container_match_conf == T | pub_match_conf == T ~ "conference",
      # Otherwise I am unable to classify with regex
      T ~ NA_character_
    ))

  return(df)
}
