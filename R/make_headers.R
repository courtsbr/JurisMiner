#' Cria vetor de headers a partir de texto com headers
#'
#' @param x String com headers
#'
#' @return Vetor nomeado para ser usado em 
#'     httr::add_headers(.header= vetor)
#' @export
#'
make_headers <- function(x){
  
  x <-   x |> 
    stringr::str_trim() |> 
    stringr::str_split("\n") |> 
    unlist() |> 
    stringr::str_split(": ")
  
  nomes <- purrr::map_chr(x, ~.x[[1]])
  
  valores <- purrr::map_chr(x, ~.x[[2]])
  
  names(valores) <- nomes
  
  return(valores)
}
