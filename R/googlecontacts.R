listar_grupos <- function(token){
  
  group_fields <- "name,metadata,clientData,groupType"
  
  
  page_size <- 1000
  
  req <- request_build(path  = "/v1/contactGroups",
                       base_url = "https://people.googleapis.com",
                       params = list(groupFields= group_fields,
                                     pageSize= page_size),
                       token  = token)
  
 resposta <- gargle::request_make(req) %>% 
             httr::content("text") %>% 
             jsonlite::fromJSON(flatten  = TRUE) %>% 
             .[[1]]
  resposta
}

pegar_contatos <- function(id_grupo, token){
  
  
  req <- request_build(path  = paste0("/v1/contactGroups/",id_grupo),
                       base_url = "https://people.googleapis.com",
                       params = list(maxMembers= 1000),
                       token  = token)
  
  resposta <- gargle::request_make(req) %>% 
    httr::content("text") %>% 
    jsonlite::fromJSON(flatten  = TRUE) %>% 
    .[["memberResourceNames"]]
  
  
  
  
}

extrair_contato_por_id <- function(id_usuario, token){
  
  if (!stringr::str_detect(id_usuario[1], "people")){
    
    id_usuario <- paste0("people/",id_usuario)
    
  }
  
  
  
 dd<-   purrr::map_dfr(id_usuario, ~{

  req <- request_build(path  = paste0("/v1/",.x),
                       base_url = "https://people.googleapis.com",
                       params = list(personFields="names,emailAddresses"),
                       token  = token)
  
  
  resposta <- gargle::request_make(req) %>% 
             httr::content("text") %>% 
             jsonlite::fromJSON(flatten = TRUE)
  
  nome_completo <- resposta$names$displayName
  email <- resposta$emailAddresses$value
  
  tibble::tibble(nome_completo, email)
  
   })
}


