#' Encontra a quantidade de processos distribuídos numa unidade judiciária.
#'
#' @param fim Qualquer inteiro que seguramente é superior ao número de processos distribuídos
#' @param inicio Se você sabe que não pode haver menos que tanto, coloca esse número
#'     para reduzir o número de buscas.
#' @param ano Indicar o ano em questão
#' @param nivel Nível do judiciário
#' @param uf Unidade federativa.
#' @param distribuidor Código do distribuídor. Encontrar no data-raw.
#' @param funcao Função a ser aplicada para baixar: esaj::download_cpopg, tjdft::baixar_processo, etc.
#' @param expr Expressão a ser avaliada. No caso de São Paulo, eu criei uma função
#'      interna chamada `sp_vazio`, que verifica se o tamanho do arquivo é menor que 90Mb.
#' 
#'  @details Essa função adata um procedimento heurístico para encontrar a quantidade de 
#'      processos distribuídos numa unidade judiciária em um determinado ano. 
#'      À moda de busca binária, ela vai iniciar requisições a partir da média entre 
#'      o ínicio e o fim indicados por você. Ela vai vai fazer cinco requisições por vez,
#'      pois  pode haver uma sequência de processos não existentes. Como cada função para baixar 
#'      processos retorna um objeto distinto, você deve indicar uma expressão que retornará 
#'      verdadeiro para casos de processos não existentes. Para São Palo, basta usar a função 
#'      interna sp_vazio. Futuramente, incluirei outras expressões para outros TJs, 
#'      de modo que este parâmetro será obsoleto.
#'      
#' @return Quantidade máxima aproximada de processos distribuídos. 
#' @export
#'
cnj_quantidade <-
  function(fim,
           inicio=0,
           ano,
           nivel,
           uf,
           distribuidor,
           funcao,
           expr = "sp_vazio") {
    ## Para encontrar o maior número do processo do ano, eu usei a lógica da busca binária.
    ## fim pode ser qualquer número grande o bastante para ser superior ao número total de processos
    ## distribuídos.
    
    inicio <- mean(c(inicio,fim)) ## Calculo a média, mas não vejo necessidade de arrendondar.
    
    # O loop abaixo faz requisição à la busca binária. Pode haver uma pequena diferença de 2.
    
    while (`-`(fim, inicio) > 2) {
      # Todas as funções para baixar htmls dos processos, de todos os pacotes,
      # possuem um argumento para o vetor de processos (ids) e outro para o
      # diretório ou path. Assim, criamos um diretorio temporário para guardar
      # os arquivos:
      
      temporario <- tempdir()
      
      ## Criamos um intervalo de cinco números em torno de y 
      ## para assegurar que ao menos um deles existirá caso o último seja
      ## superior ou igual a y.
      
      intervalo <- round(inicio + -2:2) %>%
        range()
      
      ## aqui eu uso a função cnj_sequencial para criar a numeracao conforme o CNJ,
      ## aplico a função para baixar e verifico se os cinco são simultaneamente nulos,
      ## somando os objetos lógicos. Se a soma for cinco, ou seja, TRUE, TRUE, TRUE, TRUE, TRUE
      ## o último processo é menor que inicio.
      
      soma <-
        cnj_sequencial(intervalo[1], intervalo[2], ano, nivel, uf, distribuidor) %>%
        funcao(temporario) %>%
        purrr::map_dbl(eval(parse(text = expr))) %>% ## Eu usei NULL como padrão porque a requisição para o DF retorna nulo,
        # mas isso não se aplica a outros tribunais.
        sum()
      
      unlink(temporario) ## manda o diretório pro espaço.
      
      ## Se inicio for maior que o último processo, substituímos inicio atual pelo y anterior,
      ## e fim se torna o atual inicio, isto é a média entre inicio e fim.
      ## Se o último for maior que inicio, fim é preservado e inicio passa a ser
      ## a média entre inicio e fim.
      
      if (soma == 5) {
        inicio <- inicio - (fim - inicio)
        
        fim <- mean(c(inicio,fim))
        
      } else {
        inicio <- mean(c(inicio,fim))
        
      }
      
    }
    
    return(inicio)
  }
