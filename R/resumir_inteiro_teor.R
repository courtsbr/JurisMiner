#' Resume inteiro teor de decisões judiciais
#'
#' @param x Vetor de decisões
#' @param modelo Modelo
#' @param temperatura Grau de aleatoriedade. Padrão zero.
#'
#' @return resumo da decisão
#' @export
#'
resumir_inteiro_teor <- function(x, 
                                 modelo = 'gpt-3.5-turbo-16k',
                                 temperatura = 0){

  
  taxa <- purrr::rate_delay(0.2, max_times = 3)
  
  .f <- function(y){
    
    
    prompt <- glue::glue("resuma em um breve par\u00E1grafo a decis\u00E3o
                         judicial a seguir, delimitada por tr\u00EAs ap\u00F3strofes, ```{y}```")
    
    x <- openai::create_chat_completion(
      model = modelo,
      messages = list(
        list(
          "role" = "user",
          "content" = prompt
        )),
      temperature = temperatura
    )$choices$message.content
    
    return(x)
  }
  
resumir <- purrr::insistently(.f, taxa, quiet = FALSE)
  
purrr::map_chr(x, purrr::possibly(~resumir(.x),
                                    NA_character_))

}