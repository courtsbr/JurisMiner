#' Wrapper para snakecase sem acentuação.
#'
#' @param x Converte para snakecase sem acentos.
#'
#' @return x em snakecase
#' @export
#'
snakecase <- function(x){

  snakecase::to_snake_case(x, transliterations = "Latin-ASCII")

}
