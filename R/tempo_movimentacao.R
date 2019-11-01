#' Calcula tempo entre movimentações processuais
#'
#' @param df Tibble com movimentação processual
#' @param data Coluna (bear) com datas no formato
#'     iso 8601
#'
#' @return Mesma tibble adicionada das colunas
#'     decorrencia e decorrencia acumulada
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tempo_movimentacao(df)
#' }

tempo_movimentacao <- function(df,data = data){
   
    
  
  
  data <- rlang::enexpr(data)
  
  df %>% 
    dplyr::group_by(processo) %>% 
    dplyr::mutate(anterior:=dplyr::lead(!!data),
                  decorrencia=lapso(anterior,!!data,unidade="dia"),
                  decorrencia_acumulada=tidyr::replace_na(decorrencia,0) %>% 
                    rev() %>% cumsum() %>% rev())
  
  
}