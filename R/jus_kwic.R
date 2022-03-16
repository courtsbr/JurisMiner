#' Extrai contextos de padrões (regex)
#'
#' @param x Vetor de julgados
#' @param nomes Vetor com os nomes/números dos julgados.
#' @param regex Expressão regular
#' @param compact Remover nomes/números não encontrados?
#' @param tbl Converter resultado para tabela. Esta
#'     opção remove não encontrados
#'
#' @return Lista ou tibble
#' @export
#'

jus_kwic <- function(x, nomes = names(x), regex, compact = FALSE, tbl = FALSE){

  tokenizers::tokenize_sentences(x) |>
    purrr::map(~stringr::str_subset(.x, regex)) |>
    purrr::set_names(nomes) |>
    purrr::when(compact ~ purrr::compact(.), ~.) |>
    purrr::when(tbl ~ tibble::tibble(id = names(.), trecho = .) |> tidyr::unnest(trecho), ~.)
}
