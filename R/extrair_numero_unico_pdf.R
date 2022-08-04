#' Extrai número único do pdf
#'
#' @param arquivos Vetor de arquivos.
#' @param diretorio Caso não informe arquivos.
#'
#' @return Lista com números
#' @export
#'
extrair_numero_unico_pdf <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern= "pdf$", full.names = TRUE)
    
  }
  
  purrr::map(arquivos,~{
    
    pdftools::pdf_text(.x) |> 
      paste(collapse = "\n") |> 
      stringr::str_extract_all( "\\d+-\\d{2}\\.\\d{4}\\.8.26.\\d{4}") |> 
      unlist() |> 
      unique()
  })
  
}