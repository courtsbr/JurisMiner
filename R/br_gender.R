#' Atribui sexo a nomes com base no primeiro nome a partir da base do IBGE.
#'
#' @param nomes Vetor com os nomes completos ou não para realizar a busca.
#'
#' @return tibble com cinco colunas: primeiro_nome,f para feminino, m para masculino,sexo e nomes=para o nome
#' @export
#'
br_gender <- function(nomes) {
  

  input <- nomes %>% 
    tibble::tibble(nomes=.) %>% 
    dplyr::mutate(primeiro_nome = stringr::str_extract(nomes,"\\w+"))
  
  primeiro_nome <- unique(input$primeiro_nome)
  
  
  url_f <-
    paste0(
      "https://servicodados.ibge.gov.br/api/v1/censos/nomes/basica?nome=",
      primeiro_nome,
      "&sexo=f"
    ) %>% 
    purrr::map(utils::URLencode) %>% 
    unlist()
  
  url_m <-
    paste0(
      "https://servicodados.ibge.gov.br/api/v1/censos/nomes/basica?nome=",
      primeiro_nome,
      "&sexo=m"
    ) %>% 
    purrr::map(utils::URLencode) %>% 
    unlist()  
  
  cf<-crul::Async$new(
    urls<-url_f
  )
  
  resf <- cf$get()
  
  
  cm <- crul::Async$new(
    urls <- url_m
  )
  
  resm <- cm$get()
  
  
  
  resf <- purrr::map(resf,~{
    .x$parse() %>% 
      stringr::str_match_all("(?:\\:)(\\d+)") %>%
      unlist() %>%
      magrittr::extract(6)
  }) %>% 
    unlist()
  
  resm <- purrr::map(resm,~{
    .x$parse() %>% 
      stringr::str_match_all("(?:\\:)(\\d+)") %>%
      unlist() %>%
      magrittr::extract(6)
  }) %>% 
    unlist()
  
  tibble::tibble(primeiro_nome = primeiro_nome,f= resf,m=resm) %>%
    purrr::modify_at(2:3, as.integer) %>%
    tidyr::replace_na(list(f = 0, m = 0)) %>%
    dplyr::mutate(sexo = dplyr::case_when(f > m ~ "feminino",
                                          f < m ~ "masculino",
                                          TRUE ~ "desconhecido")) %>% 
    dplyr::right_join(input,by="primeiro_nome")
  
}