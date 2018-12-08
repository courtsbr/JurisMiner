#' Classify high courts opinions by decision
#'
#' @param x data.frame with a column classified
#' @param text Column where the "dispositivo" can be found.
#' @param decision Column name to be added with the decision.
#'
#' @return same data.frame with a new column called decision.
#' @export
#'

classify_appeal <- function(x, text, decision) {
  input <- rlang::enexpr(text)
  decision_out <- rlang::enexpr(decision)
  y <- x %>%
    dplyr::distinct(rlang::UQ(input)) %>%
    dplyr::mutate(alternative = tolower(rlang::UQ(input)) %>%
                    stringi::stri_trans_general(., "latin-ascii"))
  
  y <- y %>%
    dplyr::mutate(
      rlang::UQ(decision_out) :=
        dplyr::case_when(
          stringi::stri_detect_regex(alternative, "(?=.*\\bderam\\b)(?=.*\\bneg[oa]\\w*\\b)") ~ "duvida",
          stringi::stri_detect_regex(alternative, "(?=.*\\bderam\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
          stringi::stri_detect_regex(alternative, "(?=.*\\bneg[oa]\\w*\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
          stringi::stri_detect_regex(alternative, "(?=.*\\bacolh\\w+\\b)(?=.*\\bneg[ao]\\w*\\b)") ~ "duvida",
          stringi::stri_detect_regex(alternative, "parcial\\w*\\sprovi\\w+") ~ "parcial",
          stringi::stri_detect_regex(alternative, "(nao\\sconhec\\w+|nao\\sse\\sconhec\\w+)") ~ "n\u00e3o conhecido",
          stringi::stri_detect_regex(alternative, "^desconh\\w+") ~ "desconhecido",
          stringi::stri_detect_regex(alternative, "nao\\s+conhec\\w+") ~ "desconhecido",
          stringi::stri_detect_regex(alternative, "^(desp|impr)") ~ "improvido",
          stringi::stri_detect_regex(alternative, "(nao|nega\\w+)\\s+provi\\X*") ~ "improvido",
          stringi::stri_detect_regex(alternative, "^prove\\w+") ~ "provido",
          stringi::stri_detect_regex(alternative, "^mantiveram") ~ "improvido",
          stringi::stri_detect_regex(alternative, "acolh\\w+") ~ "provido",
          stringi::stri_detect_regex(alternative, "(deram|da\\-*\\s*se|dando\\-*(se)*|comporta|\\bdou\\b|confere\\-se|se\\s*\\-*da|merece)") ~ "provido",
          stringi::stri_detect_regex(alternative,"(nao\\sderam|nao\\smerece|se\\snega|nega\\-*\\s*se|negar\\-*\\s*lhe|nao\\scomporta|negram|negararam|nego|negar)" ) ~ "improvido",
          stringi::stri_detect_regex(alternative, "(homolog|desistencia)") ~ "desist\u00eancia",
          stringi::stri_detect_regex(alternative, "(anular\\w*|nulo|nula|nulidade)") ~ "anulado",
          stringi::stri_detect_regex(alternative, "diligencia") ~ "convers\u00e3o em dilig\u00eancia",
          stringi::stri_detect_regex(alternative, "(prej|extin)") ~ "prejudicado/extinto",
          TRUE ~ "others"
        )
    ) %>%
    dplyr::select(-alternative)
  
  x %>%
    dplyr::left_join(y)
}