#' Extrai informações de textos judiciais com o GEMINI
#'
#' @param x Texto
#' @param api_key Chave do GEMINI. Se não informada, buscara a variável de ambiente
#'     GEMINI_API_KEY.
#' @param instrucao Orientação ao GEMINI, informando inclusive a delimitação por três
#'     apóstrofes.
#' @param perguntas Vetor de perguntas.
#' @param colunas Vetor de colunas.
#' @param modelo Modelo do Gemini
#' @param saida Padrão para "application/json"
#' @param temperatura Nível de aleatoriedade. Padrão: 0, ou seja, determinístico.
#' @param max_output_tokens = Número máximo na resposta.
#' @param top_p  Máxima probabilidade cumulativa para a amostra de tokens.
#' @param top_k Número máximo de tokens incluídos na amostra.
#'
#' @return json
#' @export
#'
gemini_extrair <- function(x, 
                           api_key=NULL, 
                           instrucao, 
                           perguntas, 
                           colunas,
                           modelo = "gemini-1.5-flash",
                           saida = "application/json",
                           temperatura = 1,
                           max_output_tokens = 8192,
                           top_p = 0.4,
                           top_k = 2){
  
  if(is.null(api_key)){
    
    api_key = Sys.getenv("GEMINI_API_KEY")
  }
  uri <- glue::glue("https://generativelanguage.googleapis.com/v1beta/models/{modelo}:generateContent?key={api_key}")
  
  
  headers <- c(`Content-Type`="application/json")
  
  perguntas <- stringr::str_c(perguntas, collapse = ",\n")
  
  colunas <- stringr::str_c(colunas, collapse = ", ")
  
  body <- list(contents = list(
    parts = list( text = glue::glue("{instrucao} ```{x}```. Responda \u00E0s seguintes perguntas:
{perguntas}
Retorne as respostas em formato json com as seguintes chaves: {colunas}")
    )),
    `safetySettings` = list(
      list(category = "HARM_CATEGORY_HARASSMENT",
           threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_HATE_SPEECH",
           threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_DANGEROUS_CONTENT",
           threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_SEXUALLY_EXPLICIT",
           threshold = "BLOCK_NONE")
    ),
    
    `generationConfig` = list(
      `response_mime_type` = saida,
      temperature = temperatura,
      `maxOutputTokens` =  max_output_tokens,
      topP =  top_p,
      topK = top_k
    )
    
    
  )
  
  
  taxa <- purrr::rate_delay(0.2, max_times = 3)
  
  
  .f <- function(){
    
    r2 <- httr::POST(uri, body = body, encode ="json",
                     httr::add_headers(.headers = headers))
    
    
    x1 <-  httr::content(r2, "text") |>
      jsonlite::fromJSON() |>
      purrr::pluck("candidates","content","parts",1,"text") |>
      stringr::str_extract("\\{\\X+\\}") |>
      jsonlite::prettify()
    
    return(x1)
  }
  
  
  extrair <- purrr::insistently(.f, taxa, quiet = FALSE)
  extrair()
}


#' Extrai informações de textos judiciais com o GEMINI com saída estruturada
#'
#' @param x Texto
#' @param api_key Chave do GEMINI. Se não informada, buscara a variável de ambiente
#'     GEMINI_API_KEY.
#' @param instrucao Orientação ao GEMINI, informando inclusive a delimitação por três
#'     apóstrofes.
#' @param perguntas Vetor de perguntas.
#' @param colunas lista  com json schema
#' @param modelo Modelo do Gemini
#' @param temperatura Nível de aleatoriedade. Padrão: 0, ou seja, determinístico.
#' @param max_output_tokens = Número máximo na resposta.
#' @param top_p  Máxima probabilidade cumulativa para a amostra de tokens.
#' @param top_k Número máximo de tokens incluídos na amostra.
#'
#' @return json
#' @export
#'
gemini_extrair2 <- function(x, 
                            api_key=NULL, 
                            instrucao, 
                            perguntas, 
                            colunas,
                            modelo = "gemini-1.5-flash",
                            temperatura = 1,
                            max_output_tokens = 100000,
                            top_p = 0.4,
                            top_k = 2){
  
  if(is.null(api_key)){
    
    api_key = Sys.getenv("GEMINI_API_KEY")
  }
  uri <- glue::glue("https://generativelanguage.googleapis.com/v1beta/models/{modelo}:generateContent?key={api_key}")
  
  
  headers <- c(`Content-Type`="application/json")
  
  perguntas <- stringr::str_c(perguntas, collapse = ",\n")
  
  #colunas <- stringr::str_c(colunas, collapse = ", ")
  
  body <- list(contents = list(
    parts = list( text = glue::glue("{instrucao} ```{x}```. Responda \u00E0s seguintes perguntas:
{perguntas}")
    )),
    `safetySettings` = list(
      list(category = "HARM_CATEGORY_HARASSMENT",
           threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_HATE_SPEECH",
           threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_DANGEROUS_CONTENT",
           threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_SEXUALLY_EXPLICIT",
           threshold = "BLOCK_NONE")
    ),
    
    `generationConfig` = list(
      `response_mime_type` = "application/json",
      `response_schema` = list(
        type = "OBJECT",
        properties = colunas),
      temperature = temperatura,
      `maxOutputTokens` =  max_output_tokens,
      topP =  top_p,
      topK = top_k
    )
    
    
  )
  
  
  taxa <- purrr::rate_delay(0.2, max_times = 3)
  
  
  .f <- function(){
    
    r2 <- httr::POST(uri, body = body, encode ="json",
                     httr::add_headers(.headers = headers))
    
    
    x1 <-  httr::content(r2, "text") |>
      jsonlite::fromJSON() |>
      purrr::pluck("candidates","content","parts",1,"text") |>
      stringr::str_extract("\\{\\X+\\}") |>
      jsonlite::prettify()
    
    return(x1)
  }
  
  
  extrair <- purrr::insistently(.f, taxa, quiet = FALSE)
  extrair()
}
