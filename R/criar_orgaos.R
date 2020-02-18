#' Cria sequência ordinal de órgão julgador
#'
#' @param nome ex. vara, câmara, turma
#' @param numero quantidade
#'
#' @return vetor de órgãos
#' @export
#'
criar_orgaos <- function(nome = NULL, numero = NULL ){
  
 ordinais <-c("primeira", "segunda", "terceira", "quarta", "quinta", "sexta", 
              "s\u00e9tima", "oitava", "nona", "d\u00e9cima", "d\u00e9cima primeira", "d\u00e9cima segunda", 
              "d\u00e9cima terceira", "d\u00e9cima quarta", "d\u00e9cima quinta", "d\u00e9cima sexta", 
              "d\u00e9cima s\u00e9tima", "d\u00e9cima oitava", "d\u00e9cima nona", "vig\u00e9sima", 
              "vig\u00e9sima primeira", "vig\u00e9sima segunda", "vig\u00e9sima terceira", 
              "vig\u00e9sima quarta", "vig\u00e9sima quinta", "vig\u00e9sima sexta", "vig\u00e9sima s\u00e9tima", 
              "vig\u00e9sima oitava", "vig\u00e9sima nona", "trig\u00e9sima")
 
 paste(ordinais, nome) %>% 
      .[1:numero]
  
  
}