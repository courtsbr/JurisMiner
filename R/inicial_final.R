#' Cria tibble com duas colunas de datas a partir para paralelização.
#'
#' @param data_inicial Data inicial no formato dd/mm/YYYY
#' @param data_final Data final no fomrato dd/mm/YYYY
#' @param intervalos Número de intervalos de datas criados
#' @param formato O padrão é dd/mm/YYYY, mas você pode especificar outro.
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' inicial_final(data_inicial="01/01/2017", data_final="31/12/2017")
#' }
#'
inicial_final <- function(data_inicial = NULL, data_final = NULL, intervalos=10, formato = "%d/%m/%Y"){

  tibble::tibble(datas=seq(lubridate::dmy(data_inicial),
                           lubridate::dmy(data_final),1),
                 grupos=dplyr::ntile(1:365,intervalos)) %>%
    dplyr::group_split(grupos) %>%
    purrr::map(~dplyr::pull(.x,"datas") %>%
                 range()) %>%
    do.call(rbind,.) %>%
    tibble::as_tibble() %>%
    purrr::set_names(c("data_inicial","data_final")) %>%
    dplyr::mutate_all(list(~as.Date(.,origin='1970-01-01') %>%
                             format(formato)))

}

