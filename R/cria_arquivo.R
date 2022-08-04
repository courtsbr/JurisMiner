#' Cria nome de arquivo a ser baixado
#'
#' @param diretorio Diretório
#' @param prefixo Identificador inicial
#' @param sufixo Identificador final
#' @param hora Incluir a hora da coleta?
#' @param ext Informar a extensão. Padrão é .html
#'
#' @return Nome do arquivo
#' @export
#'
#' @examples
#' cria_arquivo(diretorio = "data-raw/cjpg", prefixo = "cjpg_")
cria_arquivo <- function(diretorio = ".", prefixo = "", sufixo = "", hora = TRUE, ext = '.html'){
  
  if (hora) {
    
    file.path(diretorio, paste0(prefixo, "hora_coleta_",stringr::str_replace_all(Sys.time(),"\\D","_"), sufixo, ext))
    
  } else 
    
    file.path(diretorio, paste0(prefixo,sufixo, ext))
  
}

