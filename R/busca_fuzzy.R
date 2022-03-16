#' Procura a palavra ou frase do segundo vetor que melhor 
#'    se aproxima do primeiro. Particularmente útil para 
#'    comparar nomes de municípios.
#'    
#' @param x Vetor de strings de referência.
#' @param y Vetor de strings a serem buscados.
#'
#' @return vetor com as strings de y próximos
#'     de x.
#' @export
#'
busca_fuzzy<-function(x,y){
  
  x1 <- x |> 
    stringi::stri_trans_general("latin-ascii") |> 
    stringi::stri_trans_tolower() |> 
    stringi::stri_trim_both() |> 
    stringi::stri_replace_all_regex("\\s+","_")
  
  y1 <- y |> 
    stringi::stri_trans_general("latin-ascii") |> 
    stringi::stri_trans_tolower() |> 
    stringi::stri_trim_both() |> 
    stringi::stri_replace_all_regex("\\s+","_")
  
  purrr::map(x1, ~{
    
    if (is.na(.x)){
      
      d <- NA_character_
      
    } else {
      a <- stringdist::stringdist(.x,y1, method = 'osa',weight = c(d = 1, i = .1, s = 1, t = 1))
      
      b <- which.min(a)
      
      d <- y[b]
    }
    
   d
    
  }) |> 
    unlist()
  
}