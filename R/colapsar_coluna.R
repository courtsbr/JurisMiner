#' Colapsar coluna
#'
#' @param df data.frame
#' @param grupo grupo
#' @param coluna coluna a ser colapsada
#' @param collapse separador
#'
#' @return mesmo data.frame com a coluna colapsada
#' @export
#'
colapsar_coluna <- function(df,grupo,coluna,collapse="\n"){

  g <- rlang::enexpr(grupo)
  k <- quote(coluna)

  df <- df %>%
    dplyr::group_by(!!g) %>%
    tidyr::chop(cols := !!k)

  s <- df[[k]]

  s <- purrr::map(s,~stringr::str_c(.x,collapse="\n")) %>%
    unlist()

  df <- df %>% dplyr::mutate(!!k := !!s)

  return(df)
}
