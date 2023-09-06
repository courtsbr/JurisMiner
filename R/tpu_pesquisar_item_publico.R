#' Pesquisa ítem público das tabelas processuais unificadas
#'
#' @param tipo_tabela Tipo da tabela a ser pesquisada(A,M,C) - Assuntos, Movimentos, Classes
#' @param tipo_pesquisa Tipo da pesquisa(G,N,C) - Glossário, Nome, Código
#' @param valor_pesquisa Valor da pesquisa
#'
#' @return Tibble 
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
tpu_pesquisar_item_publico <- function(tipo_tabela, tipo_pesquisa, valor_pesquisa){
  
url1 <- "https://www.cnj.jus.br/sgt/sgt_ws.php"

corpo <- glue::glue('
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sgt="https://www.cnj.jus.br/sgt/sgt_ws.php">
<soapenv:Header/>
<soapenv:Body>
<sgt:pesquisarItemPublicoWS>
<tipoTabela>{tipo_tabela}</tipoTabela>
<tipoPesquisa>{tipo_pesquisa}</tipoPesquisa>
<valorPesquisa>{valor_pesquisa}</valorPesquisa>
</sgt:pesquisarItemPublicoWS>
</soapenv:Body>
</soapenv:Envelope>'
)
  
  
  resposta <- httr::POST(url1, body = corpo) |> 
    httr::content()
  
  item <- resposta |> 
    xml2::xml_find_all("//ns1:Item")
  
  cod_item <- item |> 
    xml2::xml_find_all(".//cod_item") |> 
    xml2::xml_text()
  
  cod_item_pai <- item |> 
    xml2::xml_find_all(".//cod_item_pai") |> 
    xml2::xml_text()
  
  nome <-  item |> 
    xml2::xml_find_all(".//nome") |> 
    xml2::xml_text()
  
  dsc_glossario <-  item |> 
    xml2::xml_find_all(".//dscGlossario/p") |> 
    xml2::xml_text()
  
  tibble::tibble(cod_item, cod_item_pai, nome, dsc_glossario)
  
}
