#' Elimina as partes indentados de textos jurídicos
#'
#' @param x Texto jurídico
#'
#' @return Mesmo texto sem partes indentadas
#' @export
#'
#' 
eliminar_textos_indentados <-  function(x){
  
  purrr::map_chr(x, eti)
  
}



eti <- function(x){
  
  
  regex <- "(\n\\s{21,})(.+)(\n\\s{21,})(.+)(\n\\s{21,})(.+)"
  
  n <- stringr::str_locate_all(x, regex)[[1]] |> 
    length()/2
  
  for (i in 1:n){
    
    l <- stringr::str_locate(x,regex)
    
    stringr::str_sub(x, l[1,1], l[1,2]) <- ""
    
  }
  
  return(x)
}
