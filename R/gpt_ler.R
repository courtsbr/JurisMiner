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

    stop("Voc\u00EA tem de fornecer o vetor de colunas")
  }

  purrr::map_dfr(arquivos, purrr::possibly(~{

    nome_arquivo <- basename(.x)
     .x |>
     readLines() |> 
      stringr::str_subset("```", negate = T) |> 
      stringr::str_c(collapse = "\n") |> 
      jsonlite::fromJSON() |>
      purrr::map_if(rlang::is_empty, \(x) NA_character_) |>
      as.data.frame() |>
      dplyr::mutate_all(as.character) |>
      dplyr::select(dplyr::any_of(colunas)) |>
      tibble::add_column(nome_arquivo, .before = 1)

  }, NULL), .progress = T)
}
