#' Extrai o ano do número do cnj
#'
#' @param processo Número do processo
#' @param tamanho 25, default, para números com ponto 
#'     e hífein,  20, para números sem.
#'
#' @return  Vetor número com pontos
#' @export
extrair_ano <- function(processo, tamanho=c(25,20)) {
  
  tamanho <- tamanho[1]
  
  if (tamanho==25){
    
    ano <-   stringr::str_sub(processo,12,15) %>% as.numeric()
  } else if (tamanho == 20){
    
    ano <-    stringr::str_sub(processo,10,13) %>% as.numeric()
    
  } else 
    stop("Tamanho n\u00e3o permitido")
  
  return(ano)
}
