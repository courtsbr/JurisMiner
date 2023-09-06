#' Pesquisa ítem público das tabelas processuais unificadas
#'
#' @param tipo_tabela Tipo da tabela a ser pesquisada(A,M,C) - Assuntos, Movimentos, Classes
#' @param cod_item Tipo da pesquisa(G,N,C) - Glossário, Nome, Código
#'
#' @return Tibble com colunas chave e valor. Além disso, argumentos de 
#'     pesquisa estão associados. Chame `str(df)` para ver os atributos.
#' @export
#'
#' @examples
#' \dontrun{
#'  # Pesquisa classe cujo código é 156
#'  df <- tpu_detalhes_item_publico(
#'                           tipo_tabela = "C",
#'                           cod_item = "156"
#'                           ) 
#' }
tpu_detalhes_item_publico <- function(tipo_tabela, cod_item){
  
  url1 <- "https://www.cnj.jus.br/sgt/sgt_ws.php"
  
  corpo <- glue::glue('
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sgt="https://www.cnj.jus.br/sgt/sgt_ws.php">
<soapenv:Header/>
<soapenv:Body>
<sgt:getArrayDetalhesItemPublicoWS>
<tipoItem>{tipo_tabela}</tipoItem>
<seqItem>{cod_item}</seqItem>
</sgt:getArrayDetalhesItemPublicoWS>
</soapenv:Body>
</soapenv:Envelope>'
  )
  
  
  resposta <- httr::POST(url1, body = corpo) |> 
    httr::content()

  chave <- resposta |> 
    xml2::xml_find_all("//key") |> 
    xml2::xml_text()
  
  valor <- resposta |> 
    xml2::xml_find_all("//value") |> 
    xml2::xml_text()
  
  
 df <- tibble::tibble(chave, valor)
 
 attr(df, "tipo_tabela") <- tipo_tabela
 attr(df, "cod_item") <- cod_item
 return(df)
}
