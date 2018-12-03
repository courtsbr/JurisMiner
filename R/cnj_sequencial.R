#' Creates cnj sequence numbers
#'
#' @param inicio integer 
#' @param fim   integer
#' @param ano   year
#' @param nivel level of the judicial system
#' @param uf  federative unity.
#' @param distribuidor code of the court.
#'
#' @return vector of lawsuit numbers
#' @export
#'
#'
#'
cnj_sequencial <- function(inicio, fim, ano, nivel, uf, distribuidor) {
  if (!is.numeric(inicio) | !is.numeric(fim)) {
    stop("inicio e fim devem ser num\u00e9ricos")
  }
  o <- stringr::str_pad(inicio:fim, width = 7, "left", "0")
  distribuidor <- stringr::str_pad(distribuidor, 4, "left", "0")
  num <- paste0(o, ano, nivel, uf, distribuidor)
  abjutils::calc_dig(num, TRUE)
}