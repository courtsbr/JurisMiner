#' Extrair ramo do assunto
#'
#' @param cod_item Código do assunto
#'
#' @returns Codigo do ramo e ramo
#' @export
#'
tpu_extrai_ramo <- function(cod_item){
  
url1 <- "https://www.cnj.jus.br/sgt/sgt_ws.php"

corpo <- glue::glue('
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sgt="https://www.cnj.jus.br/sgt/sgt_ws.php">
<soapenv:Header/>
<soapenv:Body>
<sgt:getStringPaisItemPublicoWS>
<tipoItem>A</tipoItem>
<seqItem>{cod_item}</seqItem>
</sgt:getStringPaisItemPublicoWS>
</soapenv:Body>
</soapenv:Envelope>'
)




cod_item_pai <- httr::POST(url1, body = corpo) |> 
  httr::content() |>
  xml2::xml_find_first("//return") |> 
  xml2::xml_text() |> 
  stringr::str_extract("\\d+") |> 
  JurisMiner::tpu_detalhes_item_publico("A", cod_item = _) |> 
  dplyr::filter(chave %in% c("nome","cod_item")) |> 
  dplyr::pull(valor) |> 
  t() |> 
  tibble::as_tibble() |> 
  stats::setNames(c("cod_ramo", "ramo"))
}