#' Extrai informações do GPT
#'
#' @param x Texto jurídico
#' @param instrucao Instrução para o GPT.
#' @param perguntas Vetor de perguntas. 
#' @param colunas Lista com nomes das colunas do json
#'     estruturada como no exemplo a seguir: list(resumo= list(type = "string"),
#'                                                merito = list(type = "string"
#'                                                               enum = c("procedente", "improcedente", "parcialmente procedente")),
#'                                                valor_condenacao = list(type = "number")
#'                                                )
#' @param api_key Chave OPENAI. Opcionalmente, armazene na como variável 
#'     de ambiente: OPENAI_API_KEY
#' @param modelo Modelo da OPENAI. Padrão para gpt-4o-mini.
#' @param temperatura Temperatura. Padrão para 0.
#'
#' @return json
#' @export
#'
gpt_json <- function(x,
                     instrucao = NULL,
                     perguntas, 
                     colunas,
                     api_key = NULL,
                     modelo = "gpt-4o-mini",
                     temperatura = 0
){
  
  
  if(is.null(api_key)){
    
    api_key <- Sys.getenv("OPENAI_API_KEY")
    
  }
  
  
  if (any(purrr::map_lgl(list(x, perguntas, colunas), is.null))) {
    stop("x, perguntas e colunas n\u00E3o podem ser nulos.")
  }
  if (length(perguntas) != length(colunas)) {
    stop("perguntas e colunas devem ter o mesmo tamanho.")
  }
  
  url1 <- "https://api.openai.com/v1/chat/completions"
  
  h <- c(`Authorization` = glue::glue("Bearer {api_key}"),
         `Content-Type` = 'application/json')
  
  mensagens <- json_prompt(x, instrucao, perguntas)
  
  formato_resposta <- list(
    type = "json_schema",
    json_schema = list(
      name = "jurimetria",
      schema = list(
        type = "object",
        properties = colunas,
        required = names(colunas),
        additionalProperties = FALSE
      ),
      strict = TRUE
      
    )
  )
  
  corpo <- list(model = modelo,
                messages = mensagens,
                response_format = formato_resposta,
                temperature = temperatura)
  
  url1 |> 
    httr2::request() |> 
    httr2::req_headers(!!!h) |> 
    httr2::req_body_json(corpo) |> 
    httr2::req_perform() |> 
    httr2::resp_body_json() |> 
    purrr::pluck("choices",1,"message","content")
  
  
}



#' Cria 
#'
#' @param x Texto jurídico (possivelmente)
#' @param instrucao Instrução
#' @param perguntas Vetor de perguntas
#'
#' @return lista com mensagens e instrução
#'
json_prompt <- function(x, instrucao, perguntas){
  
  
  p <- perguntas |> 
    purrr::map(~{
      list("role" = "user",
           "content" = .x)
    })
  
  
  mensagens <- list(
    list(
      "role" = "system",
      "content" = instrucao
    ),
    list(
      "role" = "system",
      "content" = glue::glue("Use o texto fornecido, delimitado por tr\u00EAs ap\u00F3strofes, ```{x}```")
      
    )
  )
  
  append(mensagens, p, after = 2)
}

