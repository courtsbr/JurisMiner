#' Keyword in context for Portuguese texts
#' 
#' \code{pt_kwic} is an usefull function that allows you to extract
#'     words placed before and after a keyword. It is similar to 
#'     \code{\link[quanteda]{kwic}} from quanteda package, with two 
#'     important differences: it is dedicated to work only with
#'     Portuguese texts in a way that ignores diacritics and
#'     is not case sensitive; also it returns each word in a
#'     separate column as default. 
#' @param string a vector of texts from which to search for the keyword.
#' @param docname a vector of docnames. If ommited, it defaults
#'     to text1,text2, text3..., 
#' @param keyword keyword to search for
#' @param type the type of keyword pattern matching: "coll" for
#'      collation, which will look for words, ignoring
#'      case differences and diacritics; "regex" for regular
#'      expressions. Regex will also ignore case differences and
#'      diacritics. Option "coll" is much faster than "regex".
#' @param before Number of words before the keyword. Default is 5
#' @param after Number of words after the keyword. Default is 5
#' @param unite if FALSE, places every previous and posterior word in
#'    separate column
#'
#' @return a tbl with docname, keyword location (start and end), the keyword,
#'     previous words, posterior words.
#' @export
#'
#' @examples
#' string<-c("A for\u00e7a do direito deve superar o direito da for\u00e7a.",
#' "Teu dever \u00e9 lutar pelo Direito, mas se um dia encontrares o Direito
#'  em conflito com a Justi\u00e7a,
#' luta pela Justi\u00e7a.")
#' docname<-c("rui_barbosa","eduardo_couture")
#' keyword<-"direito"
#' df<-pt_kwic(string,docname,keyword)



pt_kwic <- function(string,
                    docname = NULL,
                    keyword=NULL,
                    type=c("coll","regex"),
                    before = 5,
                    after = 5,
                    unite=FALSE) {
  
  if (is.null(docname)) {
    docname<-paste0("text",1:length(string))
  } else if (length(docname)==length(string)) {
    docname<-docname
  } else
    stop("string and docname must have same length")    
  
  ## if user chooses "coll", keeps the word, if "regex", rewrite the pattern
  ## to base case and diacritic insensitive.
  if (type == "coll") {
    pattern<-keyword
  } else {
    pattern <- make_pattern(keyword)
  }
  
  df<-purrr::map2_dfr(string, docname,purrr::possibly(~{
    
    ## Finds all the locations of the keyword
    if (type=="coll") {
      location<- .x %>% 
        stringi::stri_locate_all_coll(pattern,strength = 1L,locale="pt_BR") %>%
        magrittr::extract2(1) %>%
        tibble::as_tibble() %>% 
        dplyr::mutate(start=ifelse(start==1,2,start) %>% as.integer())
      
    } else {
      location<-.x %>% 
        stringi::stri_locate_all_regex(pattern) %>% 
        magrittr::extract2(1) %>% 
        tibble::as_tibble() %>% 
        dplyr::mutate(start=ifelse(start==1,2,start) %>% as.integer())
      
    }
    
    
    ## Gets the posterior words
    post<-.x %>% 
      stringi::stri_sub(location[[2]]+2,nchar(.)) %>% 
      purrr::map(~stringr::word(.x,1:after) %>% 
                   magrittr::set_names(paste0("post",1:after)))
    
    post<-dplyr::bind_rows(!!!post)
    
    ## Search for previous words. This takes a bit longer because there's a need
    ## circumvent negative location values when there are fewer words to search
    ## than asked for.
    previous<-.x %>% 
      stringi::stri_sub(1,location[[1]]-2)
    
    pre<-purrr::map2(previous,before,purrr::possibly(~{
      pre_count<-stringi::stri_count_words(.x)
      if (pre_count<.y){
        .y<-pre_count 
        
      } else {
        .y<-.y
      }
      
      stringr::word(.x,-.y:-1) %>% 
        magrittr::set_names(paste0("pre",1:.y))
      
    }, tibble::tibble()
    
    ))
    
    pre<-dplyr::bind_rows(!!!pre)
    
    ## Correct the location start back to one, when it was replaced by two.
    
    location %<>% 
      dplyr::mutate(start=ifelse(start==2,1,start) %>% as.integer())
    
    ## Extract the keywords
    
    keyword <-  .x %>% 
      stringi::stri_sub(location[[1]],location[[2]]) %>% 
      tibble::tibble(keyword=.)
    
    ## Repeats docname as many times as the keyword appears in the text.
    if (is.null(.y)) {
      docname <- paste0("text", nrow(keyword)) %>% 
        tibble::tibble(docname = .)
    } else {
      docname <- rep(.y, nrow(keyword)) %>%
        tibble::tibble(docname = .)
    }
    
    ## Binds docname, location, keywords, pre and post altogether
    dplyr::bind_cols(docname, location, pre, keyword, post)
    
  },tibble::tibble()))
  
  ## Finally, if unite is TRUE, unites columns starting with "pre" in "pre" column. Same with "post"
  if (unite==TRUE){
    df %>% tidyr::unite(post,tidyselect::vars_select(names(.),tidyselect::starts_with("post")),sep=" ") %>% 
      tidyr::unite(pre,tidyselect::vars_select(names(.),tidyselect::starts_with("pre")),sep=" ")
  } else {
    return(df)
  }
}




