
[![Build
Status](https://travis-ci.org/courtsbr/JurisMiner.svg?branch=master)](https://travis-ci.org/courtsbr/JurisMiner)

# JurisMiner

Este pacote é uma miscelânia de funções que eu fui criando a partir de
rotinas que se repetem durante a limpeza e organização de dados
jurídicos.

Por ser uma coletânia criadas em momentos distintos, algumas funções
estão em inglês, outras em português, a depender do humor do momento.

## Instalação

``` r
# install.packages("devtools")
devtools::install_github("courtsbr/JurisMiner")
```

## Uso

Sugiro ler o manual para aprender o funcionamento de cada uma das
funções. Algumas dependências não são automaticamente instaladas.
Deverão ser instaladas pelo usuário conforme necessite usar funções que
as usam.

Há algumas funções que são particularmente úteis, como
`tempo_movimentacao` e `agrupar_datas`, `busca_fuzzy`, `cnj_sequencia`,
`eliminar_textos_identados`, `jus_kwic`.
