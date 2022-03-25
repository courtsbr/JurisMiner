#' Extrair quantidade de droga da decisão sobre tráfico ou porte
#'
#' @param x Texto
#' 
#' @details Extrai somente quantidade em gramas
#' @return Quantidade em gramas da droga
#' @export
#'
extrair_quantidade_droga <- function(x){
  
  stringr::str_extract(x, "\\d[\\d\\s,.]+\\s?(?=g)") |> 
    stringr::str_remove_all("\\.") |> 
    stringr::str_replace(",", ".")
  
}

