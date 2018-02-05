#' Function clearPt
#' 
#' Optionally Removes stopwords, numbers, ordinals, whitespaces, punctuation, accents, and translate to lower case. 
#'
#'
#' @param x object to submitted to translation and removals.
#' @param lower logical: Translate to lower case.
#' @param stopwords logial: Removes portuguese-BR stopwords.
#' @param accent  logial: Encode to ASCII so that it removes all accents.
#' @param punctuation logical: Romoves punctuation.
#' @param whitespace  logical: Remove whitespaces
#' @param numbers     logical: Removes numbers
#' @param ordinal    logical: Remover indicadores de ordinais
#' @keywords clean, stopwords, accent, tranlation, punctuation, lower.
#' @import stringi
#' @import stringr
#' @import tm
#' @export
#' @examples
#' \dontrun{
#' clearPt(texto)
#' }
clearPt<-function(x,lower=T,stopwords=T,accent=T,punctuation=T,whitespace=T,numbers=T,ordinal=T){
  if (lower)
    x<-tolower(x)
  if (stopwords)
    x<=tm::removeWords(x,tm::stopwords(kind="pt"))
  if (accent)
    x<-stringi::stri_trans_general(x,"Latin-ASCII")
  if (punctuation)
    x = str_replace_all(x,"[[:punct:]]", "")
  if (whitespace) {
    x = str_replace_all(x,"\\s+", " ")
  }
  if(numbers)
    x<-str_replace_all(x,"[[:digit:]]","")
  if (ordinal)
    x<-str_replace_all(x,"(\u00ba|\u00aa)","")
  
  return(x)
}