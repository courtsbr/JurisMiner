#' Cria lista para body de requisições http
#'
#' @param query parsed query
#' @param dput TRUE para imprimir no console
#'
#' @return lista
#' @export
#'
make_body <- function(query, dput = TRUE){
paste0("http://kkk?",query) %>%
httr::parse_url() %>%
purrr::pluck("query") %>%
dput()
}