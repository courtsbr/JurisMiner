#' Find text files that contains a pattern
#'
#' @param pattern regex pattern
#' @param dir path
#' 
#' @details Specially useful for changing dependencies inside R functions.
#'
#' @return Vector of tile names
#' @export
#'
file_search <- function( pattern = "", dir = "R"){
  
  a <- list.files(dir,full.names = TRUE) 
  
  purrr::map_chr(a,~readLines(.x) %>% stringr::str_c(collapse ="\n")) %>% 
    stringr::str_which(pattern) %>% 
    a[.]
  
}