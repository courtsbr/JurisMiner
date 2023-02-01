#' Mascara coluna com url(link) com hyperlink e salva em excel.
#'
#' @param data Dataframe com coluna com o link ou url a ser mascarada
#' @param col_link Coluna com link ou url
#' @param col_texto Coluna com texto (máscara)
#' @param novo_nome Novo nome da coluna. Opcional. Se não informada
#'     receberá o mesmo nome da coluna col_link. 
#' @param arquivo Nome do arquivo que armazenará o excel.
#'
#' @return arquivo excel
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- data.frame(link = c("https://google.com","https://yahoo.com"),
#'                  nome = c("Google","Yahoo"))
#'
#' hyperlink_excel(df, col_link = link, col_texto = nome, novo_nome = url, arquivo= "meu_excel.xlsx")
#'
#' }
hyperlink_excel <- function(data, col_link, col_texto, novo_nome = NULL, arquivo){
  
  `:=` <- rlang::`:=`
  `!!` <- rlang::`!!`
  
  data1 <- rlang::enexpr(data)
  
  col_link <- rlang::enexpr(col_link)
  col_texto <- rlang::enexpr(col_texto)
  novo_nome <- rlang::enexpr(novo_nome)
  
  if (is.null(novo_nome)){
    
  novo_nome <- col_link
  
  }
  
  if (novo_nome != col_link){
  
    data <- data |>
    dplyr::mutate(!!novo_nome := paste0(
      "HYPERLINK(\"",
      !!col_link,
      "\", \"",
      !!col_texto,
      "\")"
    ))
  

    data <- data |>
    dplyr::select(!dplyr::all_of(col_link))
  
  } else {
    
    data <- data |>
      dplyr::mutate(!!col_link := paste0(
        "HYPERLINK(\"",
        !!col_link,
        "\", \"",
        !!col_texto,
        "\")"
      ))
    
  }
  
  class(data[[novo_nome]]) <- "formula"
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, data1)
  openxlsx::writeData(wb, data1, data)
  openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)
}

