#' Encontra a quantidade de processos distribuídos numa unidade judiciária.
#'
#' @param x Qualquer inteiro que seguramente é superior ao número de processos distribuídos
#' @param ano Indicar o ano em questão
#' @param nivel Nível do judiciário
#' @param uf Unidade federativa.
#' @param distribuidor Código do distribuídor. Encontrar no data-raw.
#' @param funcao Função a ser aplicada para baixar: baixar_cpopg, baixar_processo, etc.
#' @param expr Expressão a ser avaliada. A expressão deve retornar TRUE para processos
#'     não encontrados. Em São Paulo, o tamanho deve ser menor que 86K, no DF, a resposta NULL.
#'
#' @return Quantidade máxima aproximada de processos distribuídos. Pode haver um pequeno erro
#' @export
#'
#' @examples
cnj_ultimo <-
  function(x,
           ano,
           nivel,
           uf,
           distribuidor,
           funcao,
           expr = "is.null") {
    ## Para encontrar o maior número do processo do ano, eu usei a lógica da busca binária.
    ## x pode ser qualquer número grande o bastante para ser superior ao número total de processos
    ## distribuídos.
    
    y <- x / 2 ## Divido por dois para iniciar a busca binária.
    
    # O loop abaixo faz requisição à la busca binária. Pode haver uma pequena diferença de 2.
    
    while (`-`(x, y) > 2) {
      # Todas as funções para baixar htmls dos processos, de todos os pacotes,
      # possuem um argumento para o vetor de processos (ids) e outro para o
      # diretório ou path. Assim, criamos um diretorio temporário para guardar
      # os arquivos:
      
      temporario <- tempdir()
      
      ## Criamos um intervalo de três números em torno de y 
      ## para assegurar que ao menos um deles existirá caso o último seja
      ## superior ou igual a y.
      
      intervalo <- round(y+-1:1) %>%
        range()
      
      ## aqui eu uso a função cnj_sequencial para criar a numeracao conforme o CNJ,
      ## aplico a função para baixar e verifico se os três são simultaneamente nulos,
      ## somando os objetos lógicos. Se a soma for três, ou seja, TRUE, TRUE, TRUE,
      ## o último processo é menor que y.
      
      soma <-
        cnj_sequencial(intervalo[1], intervalo[2], ano, nivel, uf, distribuidor) %>%
        funcao(temporario) %>%
        purrr::map_dbl(eval(parse(text = expr))) %>% ## Eu usei NULL como padrão porque a requisição para o DF retorna nulo,
        # mas isso não se aplica a outros processos.
        sum()
      
      unlink(temporario) ## manda o diretório pro espaço.
      
      ## Se y for maior que o último processo, substituímos y atual pelo y anterior,
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
