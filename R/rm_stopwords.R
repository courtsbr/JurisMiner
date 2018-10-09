
limpar_texto<-function(x,y){
  
  y<-y %>% 
    paste0("\\b",.,"\\b","|",collapse="") %>% 
    stringr::str_replace("\\|$",")") %>% 
    stringr::str_c("(",.)
  
  x<-stringi::stri_replace_all_regex(x,y,"") %>% 
    stringr::str_trim() %>% 
    stringr::str_squish() 
}

#' Remove stopwords de decisões judiciais
#'
#' @param texto 
#' @param stopwords 
#'
#' @return Mesmo objeto texto com stopwords removidas
#' @export
#'
#' @examples
#' 
rm_stopwords<- function(texto,stopwords) {
  barra <- progress::progress_bar$new(total = length(texto))
  purrr::map(texto, ~{
    barra$tick()
    limpar_texto(.x,stopwords)
  })
}