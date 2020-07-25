#' Filtra data.frame de textos com base em regex
#'
#' @param df Data.frame
#' @param coluna Decisões judiciais
#' @param regex Regex
#' @param n Quantidade de grupos para dividir o data.frame
#'
#' @details Essa função é especialmente útil para dividir
#'     data.frames muito grandes em grupos antes de 
#'     apllicar o regex.
#' @return Mesmo data.frame filtrado
#' @export
#'
filtrar_df <- function(df = NULL,coluna = NULL, regex = NULL, n= 10){
  
  df %>% 
    dplyr::mutate(grupos = dplyr::ntile(n=n)) %>% 
    dplyr::group_split(grupos) %>% 
    purrr::map_dfr(purrr::possibly(~dplyr::filter(.x,stringr::str_detect({coluna},regex))))
  
}
