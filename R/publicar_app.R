#' Envia shinyapp para srv/shiny-server
#'
#' @param pacote Nome do pacote em que se encontra o shinyapp
#' @param nome Nome do diretório do shinyapp
#' @param destino Padrão /srv/shiny-server
#' @param host Se for localhost, envia para o servidor
#' @param port Se não for localhost, informar a porta ssh. Padrão 22.
#' @param usuario Usuário da máquina destino
#' @param senha Senha do usuário
#'
#' @return NULL
#' @export
#'
publicar_app <- function(pacote = NULL,
                         nome = NULL, 
                         destino  = "/srv/shiny-server/", 
                         host = "localhost",
                         port = 22,
                         usuario = NULL,
                         senha = NULL){

  
  
  app_dir <- system.file(nome, package=pacote)
  
  if(host == "localhost"){
  
  file.copy(
    from = app_dir,
    to =  destino,
    recursive = TRUE,
    overwrite = TRUE
  )

  } else {
    
  trecho <- paste0(usuario,"@",host,":@",porta)
  
sessao <- ssh::ssh_connect(host = trecho, password = senha )

ssh::scp_upload(sessao,app_dir, destino )

}
  
}
  