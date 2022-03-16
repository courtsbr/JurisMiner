View(tpu)
tpu <- JurisMiner::tpu
library(tidyverse)


s2 <- tpu |> 
     filter(tipo_item == 'A', cod_item =='1855') |>
     inner_join(filter(tpu, tipo_item== 'A'), by = c("cod_item"="cod_item_pai")) |> 
     inner_join(filter(tpu, tipo_item == 'A'), by = c("cod_item_pai" = 'cod_item')) |> 
     select(nome_filho =nome.y, cod_item_filho = cod_item.y, nome = nome.x, cod_item = cod_item, nome_pai = nome, cod_item_pai, cod_item_avo= cod_item_pai.y) |> 
      inner_join(filter(tpu, tipo_item =='A'), by = c("cod_item_avo"="cod_item")) |> 
      select(nome_filho, cod_item_filho, nome = nome.x, cod_item, nome_pai, cod_item_pai = cod_item_pai.x, nome_avo = nome.y, cod_item_avo, cod_item_bisa = cod_item_pai.y)



s3 <- tpu |> 
  filter(tipo_item == 'A', cod_item =='13411') |>
  inner_join(filter(tpu, tipo_item == 'A'), by = c("cod_item_pai" = 'cod_item')) |> 
  select(nome=nome.x, cod_item = cod_item, nome_pai = nome.y, cod_item_pai) |> 
  inner_join(filter(tpu, tipo_item =='A'), by = c("cod_item_pai"="cod_item")) |> 
  select(nome = nome.x, cod_item, nome_pai, cod_item_pai, cod_item, nome_pai, cod_item_pai, cod_item_avo = cod_item_pai.y, cod_item_bisa = cod_item_pai.y)


busca <- "indenização"

s <- JurisMiner::busca_fuzzy(busca,tpu$nome)



df <- tpu |> 
      filter(tipo_item == tipo)



codigo<- "13411"
busca_arvore <- function(codigos, tipo = c("A","M","C")){

  tipo <- tipo[[1]]
  
  df <- tpu |> 
        dplyr::tip
  df <-  tpu |> 
    dplyr::filter(tipo_item == tipo) 
  df1 <- df
  
 s4 <- df |>  
    dplyr::filter(cod_item == codigo) |> 
    dplyr::inner_join(df1, by = c("cod_item"="cod_item_pai")) |> 
    dplyr::inner_join(df, by = c("cod_item_pai" = 'cod_item')) |> 
    dplyr::select(nome_filho =nome.y, cod_item_filho = cod_item.y, nome = nome.x, cod_item = cod_item, nome_pai = nome, cod_item_pai, cod_item_avo= cod_item_pai.y) |> 
    dplyr::inner_join(df, by = c("cod_item_avo"="cod_item")) |> 
    dplyr::select(nome_filho, cod_item_filho, nome = nome.x, cod_item, nome_pai, cod_item_pai = cod_item_pai.x, nome_avo = nome.y, cod_item_avo, cod_item_bisa = cod_item_pai.y)
  
}

