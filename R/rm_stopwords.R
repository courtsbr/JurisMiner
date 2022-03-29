

clean_string <- function(x, y) {
  y <- y %>%
    paste0("\\b", ., "\\b", "|", collapse = "") %>%
    stringr::str_replace("\\|$", ")") %>%
    stringr::str_c("(", .)
  
  x <- stringi::stri_replace_all_regex(x, y, "") %>%
    stringr::str_trim() %>%
    stringr::str_squish()
}

#' Remove Brazilian stopwords from strings
#'
#' @param string character vector of strings
#' @param stopwords word to be removed
#'
#' @return Same object without stopwords
#' @export
#'
#'
rm_stopwords <- function(string, stopwords) {
  
  stopwords <- stopwords %>%
    paste0("\\b", ., "\\b", "|", collapse = "") %>%
    stringr::str_replace("\\|$", ")") %>%
    stringr::str_c("(", .)
  
  string <- stringi::stri_replace_all_regex(string, stopwords, "") %>%
    stringr::str_trim() %>%
    stringr::str_squish()
  
  return(string)
}

