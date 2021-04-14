#' Adiciona pontuação ao número do CNJ
#'
#' @param numero Número do cnj não pontuado. Por precaução,
#'      esta função remove caracteres não numéricos e
#'      adiciona zeros à esquerda caso não haja 20 dígitos.
#'
#' @return Número (character) devidamente pontuado
#' @export
#'
pontua_cnj <- function(numero = NULL){

  if (is.null(numero)){

    stop("Forne\u00e7a uma sequ\u00eancia num\u00e9rica")
  }

  numero %>%
     stringr::str_remove_all("\\D") %>%
     stringr::str_pad(width= 20, side = "left",pad = "0") %>%
     stringr::str_replace("(.{7})(.{2})(.{4})(.{1})(.{2})(.{4})","\\1-\\2.\\3.\\4.\\5.\\6")

}
