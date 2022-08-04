#' Extrai valor máximo arbitrado
#'
#' @param julgado Inteiro teor do julgado
#'
#' @return Tibble
#' @export
#'
extrair_valor_arbitrado <- function(julgado){
  
  julgado |> 
    stringr::str_extract_all("R\\$[\\s.]?\\d\\S+") |> 
    purrr::map_dbl(~{
      .x |> 
        stringr::str_extract(".+?(?=\\D?$)") |> 
        stringr::str_remove_all("(\\.|\\p{L}|\\$|\\s)+") |> 
        stringr::str_replace(",", ".") |> 
        as.numeric() |> 
        max(na.rm = TRUE) |> 
        unique()
    })
}
