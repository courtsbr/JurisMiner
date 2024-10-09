#' Submete decisões judiciais à  API da OPENAI e obtêm os resultados em json.
#'
#' @param x Decisisões judiciais
#' @param tipo_texto Tipo de texto
#' @param destaques Vetor de palavras ou expressões a destacar.
#' @param perguntas Vetor de perguntas.
#' @param chaves Vetor de chaves do json.
#' @param modelo Informar o modelo.
#' @param temperatura Informar a temperatura.
#'
#' @return Json
#' @export
#'
gpt_extrair <- function(x = NULL, 
                        tipo_texto = "decis\u00E3o",
                        destaques = NULL, 
                        perguntas = NULL, 
                        chaves = NULL,
                        modelo = 'gpt-4o-mini',
                        temperatura = 0){
  
  
  if (list(x, perguntas, chaves) |> purrr::map_lgl(is.null) |> any()){
    
    stop("x, perguntas e chaves n\u00E3o podem ser nulos.")
    
  }
  
  if (length(perguntas) != length(chaves)){
    
    stop("perguntas e chaves devem ter o mesmo tamanho.")
  }
  
  
  
  taxa <- purrr::rate_delay(0.2, max_times = 3)
  
  
  .f <- function(x){
    
    x1 <- openai::create_chat_completion(
      model = modelo,
      messages =  jus_prompt(x, destaques, perguntas, chaves),
      temperature = temperatura
    )$choices$message.content
    
    return(x1)
  }
  
  extrair <- purrr::insistently(.f, taxa, quiet = FALSE)
  
  purrr::map_chr(x, purrr::possibly(~extrair(.x),
                                    NA_character_))
  
}


#' Auxilia na construção de mensagens
#'
#' @param x decisão judicial
#' @param destaques Vetor de palavras ou expressões a destacar
#' @param perguntas Vetor de perguntas
#' @param chaves Vetor de chaves
#'
#' @return Lista de mensagens
#'
jus_prompt <- function(x, destaques, perguntas, chaves){
  
  
  p <- perguntas |> 
    purrr::map(~{
      list("role" = "user",
           "content" = .x)
    })
  
  chaves <- stringr::str_c(chaves, collapse = ", ")
  
  
  mensagens <- list(
    list(
      "role" = "system",
      "content" = glue::glue("Considere o texto {tipo_texto} a seguir, delimitada por tr\u00EAs ap\u00F3strofes, ```{x}```")
    ),
    list(
      "role" = "system",
      "content" = glue::glue("Retorne os resultados em formato json com as seguintes chaves: {chaves}")
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

#' Lê o json do GPT
#'
#' @param arquivos Vetor de arquivos
#' @param chaves Vetor de chaves/colunas
#'
#' @return tibble
#' @export
#'
gpt_ler_cjpg <- function(arquivos, chaves = NULL){
  purrr::map_dfr(arquivos, purrr::possibly(~{
    
    cd_doc <- stringr::str_extract(.x,"(?<=cd_doc_)[^.]+")
    
      jsonlite::fromJSON(.x) |> 
      purrr::map_if(purrr::is_empty,\(x) NA_character_) |> 
      as.data.frame() |> 
      dplyr::select(dplyr::any_of(chaves)) |> 
      dplyr::mutate_all(as.character) |> 
      tibble::add_column(cd_doc, .before = 1)
    
  }, NULL), .progress = T)
}
