#' Divide sequência em n grupos 
#'
#' @param sequencia Sequência
#' @param n Número de grupos
#'
#' @return Lista
#' @export
#'
#' @examples
#' dividir_sequencia(1:100, 5)
dividir_sequencia <- function(sequencia, n){
  
 split(sequencia, cut(seq_along(sequencia),n,labels =F))

  
}