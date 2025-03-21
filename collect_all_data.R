# Carregar pacotes
library(jsonlite)
library(glue)
library(tidyverse)


# Aumentando o tempo de espera para 120 segundos
options(timeout = 600)

# Gerar datas
datas <- seq(as.Date("2024/01/01"), as.Date("2025/01/01"), 1) |> 
  as_tibble() |> 
  mutate(year = year(value), 
         month = month(value)) |> 
  group_by(year, month) |> 
  filter(value == max(value)) |> 
  select(value) |> 
  pull()


datas <- datas[1]

### Exemplo de todos os dados: 

url <- glue("https://mananciais-sabesp.fcth.br/api/Mananciais/Boletins/Mananciais/{datas[1]}")

dados <- fromJSON(url)

### Salvando como csv: 

out <- dados[["ReturnObj"]]
names_slot <- names(out)

### Salvando todos em csv: 

for(i in 1:length(out)) {
  
  write_csv2(out[[i]], file = glue("resultados/{names_slot[i]}_{datas[1]}.csv"))
  
}
  