#' Prepara um conjunto de palavras para busca com regex
#'
#' @param x Vetor de palavras ou expressões.
#'
#' @return string para ser buscada
#' @export
#'
key_regex<-function(x = NULL){
  
  if (is.null(x)){
    
    stop("x deve ser um vetor de strings")
  }
  
  x %>% 
    stringr::str_replace("_"," ") %>%
    paste0("\\b",.,"\\b","|",collapse="") %>%
    stringr::str_replace("\\|$",")") %>%
    stringr::str_c("(",.)
  
}