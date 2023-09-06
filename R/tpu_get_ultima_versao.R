#' Verifica última versão
#'
#' @return Data com classe Date
#' @export
#'
#' @examples
#' \dontrun{
#'  # Pesquisa classe cujo código é 156
#'  tpu_pesquisar_item_publico(
#'                           tipo_tabela = "C",
#'                           tipo_pesquisa = "C",
#'                           valor_pesquisa = "156"
#'                           ) 
#' }
tpu_get_ultima_versao <- function(){
  
url1 <- "https://www.cnj.jus.br/sgt/sgt_ws.php"
  
corpo <- glue::glue('
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sgt="https://www.cnj.jus.br/sgt/sgt_ws.php">
<soapenv:Header/>
<soapenv:Body>
<sgt:getDataUltimaVersao>
</sgt:getDataUltimaVersao>
</soapenv:Body>
</soapenv:Envelope>'
)
   
    url1 |> 
    httr::POST(body = corpo) |> 
    httr::content() |> 
    xml2::xml_find_first("//return") |> 
    xml2::xml_text() |> 
    lubridate::dmy()
  
}
