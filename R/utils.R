

ifs <- function(x, y) {
  for (i in 1:length(y)) {
    x <- ifelse(stringi::stri_detect_regex(x, y[[i]][1]), y[[i]][2], x)
  }
  return(x)
}



make_pattern <- function(string) {
  list_pattern <- list(
    c("a", "(a|\u00e1|\u00e0|\u00e3)"),
    c("\u00e1", "(a|\u00e1|\u00e0|\u00e3)"),
    c("\u00e0", "(a|\u00e1|\u00e0|\u00e3)"),
    c("\u00e3", "(a|\u00e1|\u00e0|\u00e3)"),
    c("c", "(c|\u00e7)"),
    c("\u00e7", "(c|\u00e7)"),
    c("e", "(e|\u00e9|\u00ea)"),
    c("\u00e9", "(e|\u00e9|\u00ea)"),
    c("\u00ea", "(e|\u00e9|\u00ea)"),
    c("o", "(o|\u00f3|\u00f4|\u00f5)"),
    c("\u00f3", "(o|\u00f3|\u00f4|\u00f5)"),
    c("\u00f4", "(o|\u00f3|\u00f4|\u00f5)"),
    c("\u00f5", "(o|\u00f3|\u00f4|\u00f5)"),
    c("u", "(u|\u00fa|\u00fc)"),
    c("\u00fa", "(u|\u00fa|\u00fc)"),
    c("u", "(u|\u00fa|\u00fc)")
  )
  
  string %>%
    purrr::map( ~ {
      stringi::stri_split_boundaries(.x, type = "character") %>%
        magrittr::extract2(1) %>%
        ifs(list_pattern) %>%
        stringi::stri_c(collapse = "") %>%
        stringr::str_replace("_", " ") %>%
        stringr::str_c("\\b", ., "\\b", "|", collapse = "") %>%
        stringr::str_replace("\\|$", ")") %>%
        stringr::str_c("(", .)
    }) %>%
    unlist() %>%
    stringr::str_c("\\b", ., "\\b", "|", collapse = "") %>%
    stringr::str_replace("\\|$", ")") %>%
    stringr::str_c("(", .) %>% 
   stringi::stri_c("(?i)", .)
}


sp_vazio <- function(x){
  file.size(x) %>% 
    `<`(92160)
}