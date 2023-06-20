#' Resume inteiro teor de decisões judiciais
#'
#' @param x Vetor de decisões
#' @param model Modelo
#' @param temperatura Grau de aleatoriedade. Padrão zero.
#'
#' @return resumo da decisão
#' @export
#'
resumir_inteiro_teor <- function(x, 
                                 model = 'gpt-3.5-turbo-16k',
                                 temperatura = 0){
  
  
  purrr::map_chr(x, purrr::possibly(~{
    
    
    prompt <- glue::glue("resuma em um breve par\u00E1grafo a decis\u00E3o
                         judicial a seguir, delimitada por tr\u00EAs ap\u00F3strofes, ```{.x}```")
    
    x <- openai::create_chat_completion(
      model = model,
      messages = list(
        list(
          "role" = "user",
          "content" = prompt
        )),
      temperature = temperatura
    )$choices$message.content
    
  }, NA_character_))
  
}