#' \code{jurisMiner} package
#'
#' Text mining of Brazilian judicial decision
#'
#'
#' @docType package
#' @name jurisMiner
#' @importFrom magrittr %>%
#' @importFrom purrr %||%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c(".", ".x", ".y", "z", "ifs", "make_pattern",
                           "string","pre","favor_appelant","post"))
