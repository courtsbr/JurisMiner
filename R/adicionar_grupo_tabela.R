
#' Insere grupo em dataframe respeitando os cd_processos
#'
#' @param df Dataframe
#' @param by colunas: processo, cd_processo_pg etc
#' @param g Inteiro
#'
#' @returns Mesmo dataframe com a coluna informando o grupo
#'    ao qual pertence o processo ou outra coluna.
#' @export
#'
adicionar_grupo_tabela <- function(df = NULL, by = NULL, g = NULL){
  
  if(!is.integer(g)){
    
    stop("g deve ser um inteiro")
  }
  
  
  by <- rlang::ensym(by)
  df |> 
  dplyr::group_by(!!by) |> 
    dplyr::group_split() |> 
    split(g) |> 
    purrr::imap_dfr(~{
      y <- .y
      .x |> 
        purrr::map_dfr(~{
          .x |> 
            tibble::add_column(grupo = y)
          
        })
      
      
    })
  
}
