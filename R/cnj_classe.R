#' Busca códigos de classes processuais no CNJ
#'
#' @param texto Termos a serem procurados
#'
#' @return Tibble com códigos e classes
#' @export
#'
#' @examples
#' \dontrun{
#' df <- cnj_classe("procedimento comum")
#' }
cnj_classe <- function(texto = NULL)
{

  url <- "https://www.cnj.jus.br/sgt/consulta_publica_classes.php"



  body <- list(rs = "pesquisarItemGetTabela", rst = "", rsrnd = "1631957383974",
               `rsargs[]` = "C", `rsargs[]` = "N", `rsargs[]` = texto)

   conteudo <- httr::POST(url, body = body, encode = "form") %>%
     httr::content()

  codigo <- conteudo %>%
             xml2::xml_find_all("//a/@onclick") %>%
             xml2::xml_text() %>%
             stringr::str_extract("\\d+")

  texto <- conteudo %>%
           xml2::xml_find_all("//a") %>%
           xml2::xml_text() %>%
           stringr::str_remove_all(stringr::fixed("\\n")) %>%
           stringr::str_squish()

  tibble::tibble(codigo, texto)

}


