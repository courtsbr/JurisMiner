#' Creates list from parsed form data.
#'
#' @param string parsed form data.
#'
#' @details It only works with unnested form data
#' @return  list with named form's elements
#' @export
#'
#' @examples
#' string <- "visaoId=tjdf.djeletronico.comum.internet.apresentacao.VisaoDiarioEletronicoConsultaLivre&controladorId=tjdf.djeletronico.comum.internet.apresentacao.ControladorDiarioEletronicoConsultaLivre&idDoUsuarioDaSessao=&nomeDaPagina=montagem&comando=consultar&enderecoDoServlet=djeletronico&visaoAnterior=tjdf.djeletronico.comum.internet.apresentacao.VisaoDiarioEletronicoConsultaLivre&skin=&tokenDePaginacao=2&idJanelaAbrirAjax=&idJanelaFecharAjax=&idJanelaAbrirIsModalAjax=false&internet=1&textoDaConsulta=0000011-53.2016.8.07.0016&data=08%2F12%2F2018"
#' body <- create_form(string)
#'
create_form <- function(string) {
  string <- string %>%
    stringr::str_split("&") %>%
    unlist()
  
  names <- stringr::str_extract(string, ".+(?=\\=)")
  
  valores <- stringr::str_extract(string, "(?<=\\=).+") %>%
    stringr::str_replace_na("")
  
  as.list(valores) %>%
    setNames(names)
  
}
