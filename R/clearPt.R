#' Function clearPt
#' 
#' Optionally Removes stopwords, numbers, ordinals, whitespaces, punctuation, accents, and convert to lower case. 
#'
#'
#' @param string object to be submitted to conversion and removals.
#' @param lower logical Convert to lower case.
#' @param stopwords logial Removes portuguese-BR stopwords.
#' @param accent  logial remove all diacritics
#' @param punctuation logical Romoves punctuation.
#' @param whitespace  logical Remove whitespaces
#' @param numbers     logical Removes numbers
#' @param ordinal    logical Removes ordinal superscripts
#' @export
#' @examples
#' \dontrun{
#' clearPt(texto)
#' }
clearPt<-function(string,lower=T,stopwords=T,accent=T,punctuation=T,whitespace=T,numbers=T,ordinal=T){
  if (lower)
    x<-tolower(x)
  if (stopwords)
    x<=tm::removeWords(x,tm::stopwords(kind="pt"))
  if (accent)
    x<-abjutils::rm_accent(x)
  if (punctuation)
    x = stringi::stri_replace_all_regex(x,"[[:punct:]]", "")
  if (whitespace) {
    x = stringi::stri_replace_all_regex(x,"\\s+", " ")
  }
  if(numbers)
    x<-stringi::stri_replace_all_regex(x,"[[:digit:]]","")
  if (ordinal)
    x<-stringi::stri_replace_all_regex(x,"(\u00ba|\u00aa)","")
  
  return(x)
}