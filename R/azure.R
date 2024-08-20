#' Obtêm resposta da Azure Openai para peças processuais.
#'
#' @param x Vetor de documentos,e.g., julgados, petições iniciais.
#' @param tipo_texto c("decisão", "acórdão","petição",...). Informe o tipo
#'     de texto a ser analisado.
#' @param perguntas Vetor de perguntas.
#' @param colunas  Colunas/chaves das respostas em json
#' @param destaques Palavras ou expressões relevantes.
#' @param recurso Nome do recurso ou endpoint. Nome do recurso criado por você. 
#'      Você pode informar tanto o recuso quanto o endpoint que contêm o recurso.
#'      Para encontrá-lo, vá para Azure OpenAI Studio > Playground > View code.
#'      Você pode usar também a variável de ambiente: AZURE_OPENAI_RESOURCE
#' @param implementacao OU deployment id. Este valor corresponde ao nome
#'      dado por você ao implementar (deploy) um modelo. Pode ser encontrado em
#'      Resource Management > Model Deployments. 
#'      Você pode usar também a variável de ambiente: AZURE_OPENAI_IMPLEMENTACAO
#' @param versao_api Versão no formato: yyyy-mm-dd com ou sem preview.
#'      Você pode usar também a variável de ambiente: AZURE_OPENAI_VERSAO_API
#' @param api_key Chave. Você pode armazená-la na variável de ambiente
#'     AZURE_OPENAI_API_KEY.
#' @param temperatura Nível de aleatoriedade. Padrão: 0, ou seja, determinístico.
#' @param max_tokens Máximo de tokens na resposta.
#' @param presence_penalty Valores entre -2 e 2. Quanto maior o valor
#'     mais conservador (menor o número) na geração de palavras ou frases repetidas.
#' @param frequency_penalty Valores entre -2 e 2. Valores positivos aumentam a
#'     variabilidade de palavras.
#' @param logit_bias Altera a chance de um token aparecer na resposta.
#'
#' @return json
#' @export
#'
azure_openai_extrair <- function(x,
                                 tipo_texto = "decis\u00E3o",
                                 perguntas, 
                                 colunas,
                                 destaques = NULL,
                                 recurso = NULL,
                                 implementacao = NULL,
                                 versao_api = NULL,
                                 api_key = NULL,
                                 temperatura = 0,
                                 max_tokens = 4000,
                                 presence_penalty = 0,
                                 frequency_penalty = 0,
                                 logit_bias = NULL
                                 
){
  
  if (list(x, perguntas, colunas) |> purrr::map_lgl(is.null) |> any()){
    
    stop("x, perguntas e colunas n\u00E3o podem ser nulos.")
    
  }
  
  if (length(perguntas) != length(colunas)){
    
    stop("Perguntas e colunas devem ter o mesmo tamanho.")
    
  }
  

  # Prompt for information if necessary
  if (is.null(api_key)) {
    
    api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")

  }
  
  
  # Prompt for information if necessary
  if (is.null(recurso)) {
    
    recurso <- Sys.getenv("AZURE_OPENAI_RESOURCE")
    
  }
  
  
  if (is.null(implementacao)) {
    
    implementacao <- Sys.getenv("AZURE_OPENAI_IMPLEMENTACAO")
    
  }
  
  if (is.null(versao_api)) {
    
    versao_api <- Sys.getenv("AZURE_OPENAI_VERSAO_API")
    
  }
  
  
  base_url <- glue::glue("https://{recurso}.openai.azure.com/openai/deployments/{implementacao}/chat/completions?api-version={versao_api}") |> as.character()
  
  
  taxa <- purrr::rate_delay(0.2, max_times = 3)
  
  
  .f <- function(x){
    
corpo <- list(messages = azure_ai_mensagens(x, destaques = destaques, perguntas = perguntas, colunas = colunas, 0),
                  temperature = temperatura,
                  max_tokens = max_tokens,
                  presence_penalty = presence_penalty,
                  frequency_penalty = frequency_penalty,
                  logit_bias = logit_bias)
    
x1 <-  httr::POST(base_url, 
                body = corpo, 
                encode = "json", 
                httr::add_headers(`api-key`= api_key, 
                                  `Content-Type`="application/json"),
                httr::timeout(60)) |> 
      httr::content("text") |> 
      jsonlite::fromJSON() |> 
      purrr::pluck("choices","message","content")

    return(x1)
  }
  
  extrair <- purrr::insistently(.f, taxa, quiet = FALSE)
  
  purrr::map_chr(x, purrr::possibly(~extrair(.x),
                                    NA_character_))
  
}


#' Auxilia na construção de mensagens
#'
#' @param x decisão judicial
#' @param tipo_texto tipo de texto
#' @param colunas Vetor de colunas
#' @param destaques Vetor de palavras ou expressões a destacar
#' @param perguntas Vetor de perguntas
#'
#' @return Lista de mensagens
#'
azure_ai_mensagens <- function(x, tipo_texto, destaques, perguntas, colunas){
  
  
  p <- perguntas |> 
    purrr::map(~{
      list("role" = "user",
           "content" = .x)
    })
  
  colunas <- stringr::str_c(colunas, collapse = ", ")
  
  
  mensagens <- list(
    list(
      "role" = "system",
      "content" = glue::glue("Considere o texto ({tipo_texto}) a seguir, delimitado por tr\u00EAs ap\u00F3strofes, ```{x}```")
    ),
    list(
      "role" = "system",
      "content" = glue::glue("Retorne os resultados em formato json com as seguintes chaves: {colunas}")
    )
    
  )
  
  if (!is.null(destaques)){
    
    
    d <-   list(
      list(
        "role" = "system",
        "content"  = glue::glue("Sempre que poss\u00EDvel, ao elaborar o resumo, d\u00EA destaque \u00E0s seguintes palavras e express\u00F5es, delimitadas por ap\u00F3strofes e separadas por ponto e v\u00EDrgula: ```{stringr::str_c(destaques, collapse = '; ')}```")
      )
    )
    
    mensagens <- mensagens |> 
      append(d, after = 1) |> 
      append(p, after = 2)
    
  } else(
    
    mensagens = append(mensagens, p, after = 1)
    
  )
  
  return(mensagens)
}

