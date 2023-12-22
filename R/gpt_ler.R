#' Lê respostas em arquivo json do GPT
#'
#' @param arquivos Vetor de arquivos
#' @param colunas Vetor de colunas a serem lidas
#'
#' @return tibble
#' @export
#'
gpt_ler <- function (arquivos, colunas = NULL)
{

  if (is.null(colunas) | !is.vector(colunas)){

    stop("Você tem de fornecer o vetor de colunas")
  }

  purrr::map_dfr(arquivos, purrr::possibly(~{

    nome_arquivo <- basename(.x)

    .x |>
      jsonlite::fromJSON() |>
      purrr::map_if(rlang::is_empty, \(x) NA_charater_) |>
      as.data.frame() |>
      dplyr::select(dplyr::any_of(colunas)) |>
      tibble::add_column(nome_arquivo, .before = 1)

  }, NULL), .progress = T)
}