#' Search for second instance judicial decision
#'
#' @param string a vector of texts from which to search for the keyword.
#' @param id_decision a vector of id_decisions. If ommited, it defaults
#'     to text1,text2, text3..., 
#' @param keyword keyword to search for. For appeals, e.g, apelação and agravo,
#'     the keyword is usually "provimento". For writs, e.g, habeas corpus and "mandado
#'    "segurança", the keyword should be "concedida" or "concedido", or just a
#'    regex: "concedid*"
#' @param class procedural class, either "appeal" (default) or "writ".
#' 
#' @return a tbl with the id_decision, the keyword location (start and end), the keyword,
#'     the previous words, the posterior words, and the decision: c("sim","não","parcial")
#' @export
#' @examples \dontrun{
#' df<-sg_decision(string,id_decision,"provimento")
#' }

sg_decision <- function(string,id_decision,keyword,class="appeal"){
  
  df<-pt_kwic(string,id_decision,keyword,type="coll",before=9,after=9,unite=TRUE)
  
  pairs1<-"(opin.*|pelo|requer.*|parecer|manifestou|defende.*|pleite.*|pretende)"
  
  pairs2 <- list(c("deram","sim"),
                 c("da\\-*\\s*se", "sim"),
                 c("dando\\-*(se)*","sim"),
                 c("comporta.*","sim"),
                 c("dou","sim"),
                 c("confere\\-se","sim"),
                 c("se\\-*da","sim"),
                 c("merece","sim"),
                 c("nao deram","n\u00e3o"),
                 c("nao merece","merece"),
                 c("se\\snega","n\u00e3o"),
                 c("nega\\-*\\s*se", "n\u00e3o"),
                 c("negar\\-*\\s*lhe", "n\u00e3o"), 
                 c("nao\\s*comporta.*","n\u00e3o"),
                 c("negram", "n\u00e3o"),
                 c("negararam", "n\u00e3o"), 
                 c("nego", "n\u00e3o"),
                 c("negar", "n\u00e3o"))
  
  df %>%
    dplyr::mutate(pre = stringi::stri_trans_tolower(pre), 
                  pre = abjutils::rm_accent(pre)) %>% 
    dplyr::filter(!stringi::stri_detect_regex(pre, pairs1)) %>%
    dplyr::mutate(favor_appelant = ifelse(stringi::stri_detect_regex(pre,"parcial"),"parcial",pre),
                  favor_appelant = ifs(favor_appelant, pairs2)) %>% 
    dplyr::filter(favor_appelant %in% c("n\u00e3o","sim","parcial")) %>% 
    dplyr::distinct(id_decision, favor_appelant, .keep_all = TRUE)

}
