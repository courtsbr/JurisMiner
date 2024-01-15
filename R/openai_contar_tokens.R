#' Conta tokens de textos com base em modelos da OPENAI
#'
#' @param x Vetor de textos
#' @param modelo Modelo a ser utilizado.
#' 
#' @details Para usar esta função você tem de ter instalado o pacote
#'     tiktoken do Python, o qual será chamado via reticulate.
#'
#' @return Vetor com quantidade de tokens em cada texto.
#' @export
#'
openai_contar_tokens <- function(x, modelo = "gpt-3.5-turbo"){
  
  tk <- reticulate::import("tiktoken")
  
  encoding <- tk$encoding_for_model(modelo)
  
  x |> 
    purrr::map_int(purrr::possibly(~{
      
      encoding$encode(.x) |> 
        length()
    }, NA_integer_))
  
}
