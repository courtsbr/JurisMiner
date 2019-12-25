#' Prepara vetor de variáveis para aplicar juris_wider
#'
#' @param x Vetor
#'
#' @return Vetor com variáveis em minúsculo, sem acentos
#'     e espaços e pontuação substituídos por _.
#' @export
#'
reparar_vetor <- function(x){
  
  x %>% 
    stringr::str_trim() %>% 
    stringr::str_replace_all("\\W+(?!$)","_") %>% 
    tolower() %>% 
    stringi::stri_trans_general("latin-ascii")
  
}