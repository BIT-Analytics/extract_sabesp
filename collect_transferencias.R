# Carregar pacotes
library(jsonlite)
library(glue)
library(tidyverse)


# Aumentando o tempo de espera para 120 segundos
options(timeout = 600)

# Gerar datas
datas <- seq(as.Date("2010/01/01"), as.Date("2025/03/01"), 1) |> 
  as_tibble() |> 
  mutate(year = year(value), 
         month = month(value)) |> 
  group_by(year, month) |> 
  filter(value == max(value)) |> 
  select(value) |> 
  pull()


# Função para buscar dados e armazenar em data.frame
# Função para buscar dados e armazenar em data.frame
dados_coletados <- purrr::map_dfr(datas, function(data) {
  
  # Definir a URL do JSON
  url <- glue("https://mananciais-sabesp.fcth.br/api/Mananciais/Boletins/Mananciais/{data}")
  
  # Ler e converter o JSON
  dados <- fromJSON(url)
  
  # Extrair dados dos sistemas
  
  out <- dados[["ReturnObj"]][["dadosTransferencias"]] |> 
    filter(Abreviatura %in% c("Q PS-SC", "Q Rev.Capiv.", 
                              "Q Rev.Taquac.", "Q RP-RG", 
                              "Q Guarat.", "Q EEAB Bir", "Q RG-Taiaçupeba", 
                              "Q Transf Guaió")) |> 
    select(Abreviatura, Valor, SistemaId, ) |> 
    rename(id_sistema = SistemaId)
  
  # Extrair dados da ETA
  dados_transferencia <- data.frame(out) |> 
    mutate(Sistema = case_when(
      id_sistema == 0 ~ "Cantareira", 
      id_sistema == 1 ~ "Alto Tietê", 
      id_sistema == 2 ~ "Guarapiranga", 
      id_sistema == 4 ~ "Rio Grande", 
      id_sistema == 5 ~ "Rio Claro"
    ))
  
  return(dados_transferencia)
})





