#' Classifies high courts opinions
#'
#' This function will carry out a search for a keyword such as "affirm" or "reverse",
#'   but in Portuguese, from hight courts opinions already read to R and do a first attempt 
#'   to classify them 
#'
#' @param string a vector of texts from which to search for the keyword.
#' @param id_decision a vector of id_decisions. If ommited, it defaults
#'     to text1,text2, text3..., 
#' @param keyword keyword to search for. For appeals, e.g, apelação and agravo,
#'     the keyword is usually "provimento". For writs, e.g, habeas corpus and "mandado
#'    "segurança", the keyword should be "concedida" or "concedido", or just a
#'    regex: "concedid*"
#' @param class procedural class, either "appeal" (default) or "writ".
#' @param type It can be either "coll" for collation search or "regex". 
#' 
#' @return a tbl with the id_decision, the keyword location (start and end), the keyword,
#'     the previous words, the posterior words, and the decision: c("sim","não","parcial")
#' @export
#' @examples \dontrun{
#' df<-sg_decision(string,id_decision,"provimento")
#' }

sg_decision <- function(string,id_decision,keyword,class="appeal",type="coll"){
  
  df<-pt_kwic(string,id_decision,keyword,type=type,before=9,after=9,unite=TRUE)
  
  
  df %>% 
    dplyr::mutate(pre = stringi::stri_trans_tolower(pre),
                  pre = abjutils::rm_accent(pre)) %>%
    dplyr::filter(!stringi::stri_detect_regex(pre,"(opin.*|pelo|requer.*|parecer|manifestou|defende.*|pleite.*|pretende)")) %>% ## Exclui manifesta\u00e7\u00f5es do MP
    dplyr::mutate(favor_appelant = ifelse(stringi::stri_detect_regex(pre,"(deram|da\\-*\\sse|dando\\-*(se)*|comporta|dou|confere\\-se|se\\s*\\-*da|merece)"),"sim",pre), ## classifica como "sim" quando aparecem essas palavras
                  favor_appelant = ifelse(stringi::stri_detect_regex(pre,"nao\\sderam|nao\\smerece|se\\snega|nega\\-*\\s*se|negar\\-*\\s*lhe|nao\\scomporta|negram|negararam|nego|negar"),"n\u00e3o",favor_appelant),   ## classifica como "n\u00e3o" quando aparecem essas palavras             
                  favor_appelant = ifelse(stringi::stri_detect_regex(pre,"parcial"), "parcial", favor_appelant), ## classifica como parcial 
                  favor_appelant = ifelse(stringi::stri_detect_regex(post,"parcial"),"parcial",favor_appelant)) %>% ## classifica como parcial
    dplyr::filter(favor_appelant %in% c("n\u00e3o","sim", "parcial")) %>% ## Seleciona somente o que foi poss\u00edvel classificar
    dplyr::distinct(id_decision, favor_appelant,.keep_all = TRUE)
  
}