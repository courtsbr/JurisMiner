#' Colapsar as partes para visualização com DT
#'
#' @param df Data.frame
#' @param grupo Coluna agrupadora, geralmente processo
#' @param ... Colunas a serem aninhadas e colapsadas 
#' @param collapse separador dos resultados. Coloquei
#'     "<br>" pq uso no datatable.
#'
#' @return Mesmo def com valores aninhados e colapsados
#' @export
#'
colapsar_partes <- function(df,grupo,...,collapse="<br>"){
  
  g <- rlang::enexpr(grupo)  
  
  df <- df %>% 
    tidyr::unite(parte,-(!!g),sep=" ")
  
  df <- df %>% 
    dplyr::group_by(!!g) %>% 
    tidyr::chop(cols=parte)
  
  df$parte <- purrr::map(df$parte,~stringr::str_c(.x,collapse=collapse))
  
  return(df)
}
