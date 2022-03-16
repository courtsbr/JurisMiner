#' Classifica antecedência, primariedade e reincidência
#'
#' @param x Vetor de julgados ou trechos
#'     extraídos com jus_kwic
#'
#' @return Vetor com classificações
#' @export
#'
#' @examples
#' julgados <-
#' c(`1` = "Possui maus antecedentes. Julgado procedente",
#'  `2` = "Fulano não é primário.
#'      Condeno Fulano a 10 anos de reclusão.
#'      Deve recorrer em liberdade")
#'
classifica_antecedentes <- function(x){

  dplyr::case_when(
    stringr::str_detect(x, "(?i)primariedade") ~ "prim\u00e1rio",
    stringr::str_detect(x, "(?i)ostent\\w+\\s+bons\\s+anteceden") ~ "bons",
    stringr::str_detect(x, "(?i)maus\\s+antecedentes") ~ "maus",
    stringr::str_detect(x, "(?i)(?=.*\\bfavor.ve[li])(?=.*(suficiente|despiciendo|bastante|cond.o|autoriza|imped|obsta|motivo|ensej|embora|irrelevante))") ~ "bons",
    stringr::str_detect(x, "(?i)desfavor.ve") ~ "maus",
    stringr::str_detect(x, "(?i)n.o\\s+ostent\\w+\\s+antecedente") ~ "bons",
    stringr::str_detect(x, "(?i)sem\\s+antecedente") ~ "bons",
    stringr::str_detect(x, "(?i)sem\\s+ostent\\w+\\s+anteceden") ~ "bons",
    stringr::str_detect(x, "(?i)n.o\\s+registra.*antecedentes") ~ "bons",
    stringr::str_detect(x, "(?i)registra.+antecedentes") ~ "maus",
    stringr::str_detect(x, "(?i)n.o\\s+possu.+antecedentes") ~ "bons",
    stringr::str_detect(x, "(?i)bons\\s+antecedentes") ~ "bons",
    stringr::str_detect(x, "(?i)possu.+antecedentes") ~ "maus",
    stringr::str_detect(x, "(?i)n.o\\s+\\w{1,10}\\s+prim\u00e1r") ~ "reincidente",
    stringr::str_detect(x, "(?i)prim[\u00e1a]ri[ao]") ~ "prim\u00e1rio",
    stringr::str_detect(x, "(?i)contum[\u00e1a]") ~ "reincidente",
    stringr::str_detect(x, "(?i)ficha\\s+criminal") ~ "reincidente",
    stringr::str_detect(x, "(?i)reincidente") ~ "reincidente",
    stringr::str_detect(x, "(?i)\\bfavor.veis") ~ "favor\u00e1veis",
    stringr::str_detect(x, "(?i)folha")  ~ "maus",
    stringr::str_detect(x, "(?i)\\bexist.ncia") ~ "bons",
    stringr::str_detect(x, "(?i)inexist\u00eancia") ~ "maus",
    stringr::str_detect(x, "(?i)imaculado") ~ "bons",
    stringr::str_detect(x, "(?i)desabona") ~ "maus",
    stringr::str_detect(x, "(?i)reitera\u00e7\u00e3o\\s+delit") ~ "reitera\u00e7\u00e3o",
    stringr::str_detect(x, "(?i)reitera\u00e7\u00e3o\\s+(delit|crim)") ~ "reitera\u00e7\u00e3o",
    stringr::str_detect(x, "(?i)reitera\u00e7\u00e3o\\s+de\\s+(pr\u00e1tica|conduta)") ~ "reitera\u00e7\u00e3o",
    TRUE ~ NA_character_

  )



}
