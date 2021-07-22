#' Pontua cnpj ou cpf
#'
#' @param x Vetor de cnpjs e ou cpfs.
#'
#' @return Mesmo vetor com valores pontuados
#' @export
#'
#' @examples
#' pontua_cpf_cnpj(c("23268449000130","12345678901"))
pontua_cpf_cnpj <- function(x){

  x <- x %>%
    stringr::str_remove_all("\\D")

  purrr::map_chr(x,purrr::possibly(~{

    if (nchar(.x) == 14){

      stringr::str_replace(.x,"(\\d{2})(\\d{3})(\\d{3})(\\d{4})(\\d{2})","\\1.\\2.\\3/\\4-\\5")

    } else {

      stringr::str_replace(.x, "(\\d{3})(\\d{3})(\\d{3})(\\d{2})","\\1.\\2.\\3-\\4")

    }


  },NA_character_))

}
