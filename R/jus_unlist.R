#' Convert to vector list that contains empty or NULL
#'
#' @param list List containing empty values
#' @param default Default is NA_character_
#'
#' @return Vector with the same length of the list
#' @export
#'
jus_unlist <- function(list = NULL, default = NA_character_){
  
  purrr::map(list,~vctrs::`%0%`(.x,default)) %>% 
    unlist()
  
}