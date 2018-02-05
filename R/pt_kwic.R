#' Keyword in context for Portuguese texts
#' 
#' \code{pt_kwic} is an useful function that allows you to extract
#'     words placed before and after a keyword. It is similar to 
#'     \code{\link[quanteda]{kwic}} from quanteda package, with two 
#'     important differences: it is dedicated to work only with
#'     Portuguese texts in a way that ignores diacritics and
#'     is case insensitive; also it returns each word in a
#'     separate column as a default. 
#' @param string a vector of texts from which to search for the keyword.
#' @param id_decision a vector of id_decisions. If ommited, it defaults
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
#' @return a tbl with id_decision, keyword location (start and end), the keyword,
#'     the previous words, and the posterior words.
#' @export
#'
#' @examples
#' string<-c("A força do direito deve superar o direito da força.",
#' "Teu dever é lutar pelo Direito, mas se um dia encontrares o Direito
#'  em conflito com a Justiça,
#' luta pela Justiça.")
#' id_decision<-c("rui_barbosa","eduardo_couture")
#' keyword<-"direito"
#' df<-pt_kwic(string,id_decision,keyword)
pt_kwic <-
  function (string,
            id_decision = NULL,
            keyword = NULL,
            type = "coll",
            before = 5,
            after = 5,
            unite = FALSE)
  {
    if (is.null(id_decision)) {
      id_decision <- paste0("text", 1:length(string))
    } else if (length(id_decision) == length(string)) {
      id_decision <- id_decision
    } else
      stop("string and id_decision must have same length")
    if (type == "coll") {
      pattern <- keyword
    } else {
      pattern <- make_pattern(keyword)
    }
    df <- purrr::map2_dfr(string, id_decision, purrr::possibly( ~ {
      .x <- stringi::stri_replace_all_regex(.x, "\\s+", " ")
      if (type == "coll") {
        location <- .x %>% stringi::stri_locate_all_coll(pattern, strength = 1L, locale = "pt_BR") %>%
          magrittr::extract2(1) %>%
          tibble::as_tibble() %>%
          dplyr::mutate(start = ifelse(start == 1, 2, start) %>% as.integer())
      } else {
        location <- .x %>% stringi::stri_locate_all_regex(pattern) %>%
          magrittr::extract2(1) %>%
          tibble::as_tibble() %>%
          dplyr::mutate(start = ifelse(start == 1, 2, start) %>% as.integer())
      }
      post <-
        .x %>% stringi::stri_sub(location[[2]] + 2, nchar(.)) %>%
        purrr::map( ~ stringr::word(.x, 1:after) %>% magrittr::set_names(paste0("post",
                                                                                1:after)))
      post <- dplyr::bind_rows(!!!post)
      
      previous <- .x %>% stringi::stri_sub(1, location[[1]]-2)
      pre <- purrr::map2(previous, before, purrr::possibly( ~ {
        pre_count <- stringi::stri_count_words(.x)
        if (pre_count < .y) {
          .y <- pre_count
        } else {
          .y <- .y
        }
        stringr::word(.x,-.y:-1) %>% magrittr::set_names(paste0("pre",1:.y))
      }, tibble::tibble()))
      
      pre <- dplyr::bind_rows(!!!pre)
      
      location %<>% dplyr::mutate(start = ifelse(start == 2,1, start) %>% as.integer())
      
      keyword <-
        .x %>% stringi::stri_sub(location[[1]], location[[2]]) %>%
        tibble::tibble(keyword = .)
      if (is.null(.y)) {
        id_decision <-
          paste0("text", nrow(keyword)) %>% tibble::tibble(id_decision = .)
      } else {
        id_decision <- rep(.y, nrow(keyword)) %>% tibble::tibble(id_decision = .)
      }
      dplyr::bind_cols(id_decision, location, pre, keyword, post)
    }, tibble::tibble()))
    if (unite == TRUE) {
      df %>% tidyr::unite(post,tidyselect::vars_select(names(.),tidyselect::starts_with("post")),sep = " ") %>%
        tidyr::unite(pre,tidyselect::vars_select(names(.),tidyselect::starts_with("pre")),sep = " ")
    } else {
      return(df)
    }
  }



