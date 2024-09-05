#' Wrapper de pdftools::pdf_text
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Alternativamente indicar onde se encontram os arquivos
#' @param combinar Combinar as páginas num escalar? Padrão combinar
#' @param basename Cominho completo ou apenas o nome do arquivo. Padrão apenas nome.
#'
#' @return tibble
#' @export
#'
ler_pdfs <- function(arquivos = NULL, diretorio = ".", combinar = TRUE, basename = TRUE){


  if(is.null(arquivos)) {
    arquivos <- list.files(diretorio, full.names = TRUE,
                           pattern = "pdf$")
  }


  purrr::map_dfr(arquivos, purrr::possibly(~{



    suppressMessages({
      texto <- pdftools::pdf_text(.x)
    })


    if(combinar) {
      texto <- stringr::str_c(texto, collapse = "\n")
    }

    arquivo <- .x

    if(basename){

      arquivo <- basename(arquivo)
    }

    tibble::tibble(arquivo = arquivo, texto = texto)

  }, NULL), .progress = TRUE)


}
