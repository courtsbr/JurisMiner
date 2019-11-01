#' \code{jurisMiner} package
#'
#' Text mining of Brazilian judicial decisions
#'
#'
#' @docType package
#' @name jurisMiner
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") 
  
  utils::globalVariables(c(".", ".x", ".y", "x","y","post","pre", "ifs",
      "make_pattern", "string","text","decision","clean_string","texto",
      "stopwords","alternative","start","end","grupos","decorrencia","processo","anterior"))
