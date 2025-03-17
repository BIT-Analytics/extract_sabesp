
# Carregar o pacote jsonlite
library(jsonlite)
library(glue)
library(tidyverse)

# Definir o URL do JSON
url <- "https://mananciais-sabesp.fcth.br/api/Mananciais/Boletins/Mananciais/2025-02-28"

# Ler e converter o JSON em um data frame
dados <- fromJSON(url)

# Explorar a estrutura dos dados
str(dados)
head(dados)


### Olhando tabelas:

aux <- dados[["ReturnObj"]][["dadosSistemas"]]

### Salvando como csv: 

out <- dados[["ReturnObj"]]
names_slot <- names(out)

### Salvando todos em csv: 

for(i in 1:length(out)) {
  
  write.csv(out[[i]], file = glue("{names_slot[i]}_28022025.csv"))
  
  
}
