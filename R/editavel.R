#' Criar tabela para edição manual
#'
#' @param x dataframe
#' @param coluna coluna de referência
#' @param acao coluna indicando acao  a ser tomada
#' @param tamanho_pagina padrão é cem, mas você pode aumentar.
#' @param destino diretorio/arquivo onde você quer salvar o html
#'     editável. Se não indicar, será salvo um arquivo chamdo
#'     editavel.html no atural diretorio.
#' @details Esta função é especialmente importante quando queremos
#'     trabalhar em colaboração, pois ela gera um DT editável.
#'
#' @return arquivo html que pode ser aberto por qualqueer navegador.
#' @export
#'
editavel <- function(

  x=NULL,
  coluna=NULL,
  acao="     ",
  tamanho_pagina=100,
  destino="./editavel.html"

) {

  col <- rlang::enexpr(coluna)

  dplyr::count(x,!!col, sort=TRUE) %>%
    dplyr::mutate(acao=rlang::UQ(acao)) %>%
    DT::datatable(editable=TRUE,
                  extensions = 'Buttons',
                  options=list(
                    dom = 'Bfrtip',
                    buttons = c('excel'),
                    pageLength=tamanho_pagina)
    ) %>%
    DT::saveWidget(destino)


}