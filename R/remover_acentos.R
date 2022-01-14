#' Remove acentos
#'
#' @param x Vetor de textos
#'
#' @return Mesmo texto sem acentos
#' @export
#'
remover_acentos <- function(x){

x |>
    stringi::stri_trans_general("latin-ascii")


}
