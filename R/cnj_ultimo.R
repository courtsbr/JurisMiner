#' Encontra a quantidade de processos distribuídos numa unidade judiciária.
#'
#' @param x Qualquer inteiro que seguramente é superior ao número de processos distribuídos
#' @param ano Indicar o ano em questão
#' @param nivel nível do judiciário
#' @param uf unidade federativa.
#' @param distribuidor código do distribuídor. Encontrar no data-raw.
#' @param funcao Função a ser aplicada para baixar: baixar_cpopg, baixar_processo, etc.
#'
#' @return Quantidade máxima aproximada de processos distribuídos. Pode haver um erro, para cima,
#'     de cinco processos.
#' @export
#'
#' @examples
cnj_ultimo <- function(x, ano, nivel, uf, distribuidor, funcao) {
  ## Para encontrar o maior número do processo do ano, eu usei a lógica da busca binária.
  ## x pode ser qualquer número, grande o bastante para ser superior ao número total de processos
  ## distribuídos.
  
  y <- x / 2 ## Divido por dois para iniciar a busca binária.
  
  # O loop abaixo faz requisição com busca binária até encontrar
  ## o último dentro de intervalo de 5. Esse cinco é arbitrário.
  
  while (`-`(x, y) > 5) {
    # Todas as funções para baixar htmls dos processos, de todos os pacotes,
    # possuem um argumento para o vetor de processos (ids) e outro para o
    # diretório ou path. Assim, criamos um diretorio temporario para guardar
    # os arquivos:
    
    temporario <- tempdir()
    
    ## Criamos um intervalo em torno de y em torno de três números.
    ## para assegurar ao menos um deles existirá caso o último seja
    ## superior ou igual a y.
    
    intervalo <- round(y+-1:1) %>%
      range()
    
    ## aqui eu uso a função cnj_sequencia para criar a numeracao conforme o CNJ,
    ## aplico a função para baixar e verifico se os três são simultaneamente nulos,
    ## somando os objetos lógicos. Se a soma for três, ou seja, TRUE, TRUE, TRUE,
    ## o último processo é menor que y.
    
    soma <-
      cnj_sequencial(intervalo[1], intervalo[2], ano, nivel, uf, distribuidor) %>%
      funcao(temporario) %>%
      purrr::map_dbl(is.null) %>% ## Eu usei NULL porque a requisição para o DF retorna nulo,
      # mas isso não se aplica a outros processos
      sum()
    
    unlink(temporario) ## manda o diretório pro espaço.
    
    ## Se y for maior que o último processo, igualamos y ao y anterior,
    ## e x se torna o atual y, isto é a mediana entre y e x.
    ## Se o último for maior que y, x é preservado e y passa a ser
    ## a mediana entre y e x.
    
    if (soma == 3) {
      y <- y - (x - y)
      
      x <- median(y:x)
      
    } else {
      y <- median(y:x)
      
    }
    
  }
  
  return(y)
}
