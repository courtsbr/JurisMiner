#' Obter lista de magistrados
#'
#' @param uf Unidade Federativa, e.g., "SP"
#' @param justica Justica estadual (1) ou federal (2)
#' @param grau Somente primeiro grau funciona
#'
#' @return Dataframe com nomes de magistrados e
#'     tribunal
#' @export
#'
obter_magistrados <- function(uf, justica = 1, grau = 1 ){


  if (grau != 1){

    stop("Somente primeiro grau")
  }

  body <- list(
    d = "consulta",
    a = "consulta",
    f = "formPrincipalListaMagistrado",
    token = "",
    uf = uf,
    tipo_justica = justica
  )

  url <- "https://www.cnj.jus.br/corregedoria/justica_aberta/"

  conteudo <- httr::POST(url, body = body, encode = "form") |>
    httr::content()


  magistrado <-  conteudo |>
    xml2::xml_find_all("//tr/td[1]")|>
    xml2::xml_text()

  tribunal <- conteudo |>
    xml2::xml_find_all("//tr/td[2]")|>
    xml2::xml_text()

  tibble::tibble(magistrado, tribunal)
}
