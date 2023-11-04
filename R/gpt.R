#' Submete decisões judiciais à API da OPENAI e obtêm os resultados em json.
#'
#' @param x Decisão judicial
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
                        destaques = NULL, 
                        perguntas = NULL, 
                        chaves = NULL,
                        modelo = 'gpt-3.5-turbo-16k',
                        temperatura = 0){
  
  
  if (list(x, perguntas, chaves) |> purrr::map_lgl(is.null) |> any()){
    
    stop("x, perguntas e chaves não podem ser nulos.")
    
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
      "content" = glue::glue("Considere a decisão a seguir, delimitada por três apóstrofes, ```{x}```")
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
      "content"  = glue::glue("Sempre que possível, ao elaborar o resumo, dê destaque às seguintes palavras e expressões, delimitadas por apóstrofes e separadas por ponto e vírgula: ```{stringr::str_c(destaques, collapse = '; ')}```")
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
