#' Divide sequência em n grupos 
#'
#' @param sequencia Sequência
#' @param g Número de grupos.
#' @param n Alternativamente número de elementos por grupo.
#' @return Lista
#' @export
#'
#' @examples
#' dividir_sequencia(1:100, 5)
dividir_sequencia <- function(sequencia, g = NULL, n = NULL){
  
   if (!is.atomic(sequencia)){
       
      stop("sequencia deve ser um vetor (at\u00F4mico)")

   }

  if (!is.null(g) & !is.null(n)){
    
    stop("Voc\u00EA deve informar g ou n, nunca os dois.")
  }
  
  if (is.null(g) & is.null(n)){
    
    stop("Voc\u00EA tem de informar g ou n")
  }
  
 if (!is.null(g)){
   
  grupos <-  split(sequencia, cut(seq_along(sequencia),g,labels =F))
   
  return(grupos)
 }

 if (!is.null(n)){
   
   numero <- split(sequencia, ceiling(seq_along(sequencia)/n))
   
   return(numero)
 }
  
}