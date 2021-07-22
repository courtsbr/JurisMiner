#' Lista arquivos rapidamente
#'
#' @param diretorio Diretório onde se encontram os arquivos
#' 
#' @details Esta função tem o propósito de listar arquivos
#'     rapidamente, não ordenados. Não é recursiva, não aceita
#'     regex. Use list.files do base se forem poucos arquivos
#'     ou necessita de mais argumentos. 
#'
#' @return Vetor de arquivos com caminho absoluto
#' @export
#'

listar_arquivos <- function(diretorio = "."){
  
  path <- normalizePath(diretorio)
  
  files <-  sys::exec_internal("ls", c("-u",path))$stdout
  
  file.path(path,sys::as_text(files))
  
  
}
