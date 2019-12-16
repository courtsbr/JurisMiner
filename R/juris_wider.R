#' Converte para formato wide
#'
#' @param df tibble
#' @param valor valor
#' @param variavel variavel
#' @return tibble
#' @export
#'
juris_wider <- function(df,valor, variavel){
  
  
  df %>% 
    dplyr::group_by_at(dplyr::vars(-valor)) %>%
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = variavel, value = valor) %>%
    dplyr::select(-row_id)
  
  
}