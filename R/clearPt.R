#' Optionally removes stopwords, numbers, ordinals, whitespaces, punctuation, diacritics, and convert to lower case. 
#'
#'
#' @param string object to be submitted to conversion and removals.
#' @param lower logical Converts to lower case.
#' @param stopwords logial Removes portuguese-BR stopwords.
#' @param diacritics  logial remove all diacritics
#' @param punctuation logical Romoves punctuation.
#' @param squish  logical applies stringr::str_squish
#' @param numbers     logical Removes numbers
#' @param ordinal    logical Removes ordinal superscripts
#' @export
#' @examples
#' \dontrun{
#' clearPt(texto)
#' }
clearPt <- function(string,
                  lower=TRUE,
                  stopwords=TRUE,
                  diacritics=TRUE,
                  punctuation=TRUE,
                  squish=TRUE,
                  numbers=TRUE,
                  ordinal=TRUE) {
  
 string <- dplyr::case_when(
    lower==TRUE ~ tolower(string),
    stopwords==TRUE ~ rm_stopwords(string),
    diacritics == TRUE ~ abjutils::rm_accent(string),
    punctuation == TRUE ~ stringi::stri_replace_all_regex(string,"[[:punct:]]+", ""),
    numbers == TRUE ~ stringi::stri_replace_all_regex(string,"[[:digit:]]+",""),
    ordinal == TRUE ~ stringi::stri_replace_all_regex(string,"(\\u00ba|\\u00aa)+",""),
    TRUE ~ string
  )
  

  return(string)
}