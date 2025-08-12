# Faz o download das transferências

# Carregar pacotes
library(jsonlite)
library(glue)
library(tidyverse)


# datas ja baixadas: 

df <- read_csv("resultados/transferencias.csv")

datas_baixadas <- df |> 
  distinct(data) |> 
  pull() |> 
  as.Date()

# Aumentando o tempo de espera para 120 segundos
options(timeout = 600)

# Gerar datas
datas <- seq(as.Date("2010/01/01"), as.Date("2025/07/30"), 1) |> 
  as_tibble() |> 
  mutate(year = year(value), 
         month = month(value)) |> 
  group_by(year, month) |> 
  filter(value == max(value)) |> 
  select(value) |> 
  pull()

# datas faltantes

data_download <- as.Date(setdiff(datas, datas_baixadas))


# Função para buscar dados e armazenar em data.frame
# Função para buscar dados e armazenar em data.frame
# Criar função resiliente com insistently()
safe_fromJSON <- insistently(fromJSON)

dados_coletados <- purrr::map_dfr(data_download, function(data) {
  
  # Definir a URL do JSON
  url <- glue::glue("https://mananciais-sabesp.fcth.br/api/Mananciais/Boletins/Mananciais/{data}")
  
  # Ler e converter o JSON com tentativa automática de repetição
  dados <- safe_fromJSON(url)
  
  # Extrair dados dos sistemas
  out <- dados[["ReturnObj"]][["dadosTransferencias"]] |> 
    dplyr::filter(Abreviatura %in% c("Q PS-SC", "Q Rev.Capiv.", 
                                     "Q Rev.Taquac.", "Q RP-RG", 
                                     "Q Guarat.", "Q EEAB Bir", "Q RG-Taiaçupeba", 
                                     "Q Transf Guaió")) |> 
    dplyr::select(Abreviatura, Valor, SistemaId) |> 
    dplyr::rename(id_sistema = SistemaId) |> 
    mutate(data = data)
  
  # Extrair dados da ETA
  dados_transferencia <- dplyr::mutate(out, Sistema = dplyr::case_when(
    id_sistema == 0 ~ "Cantareira", 
    id_sistema == 1 ~ "Alto Tietê", 
    id_sistema == 2 ~ "Guarapiranga", 
    id_sistema == 4 ~ "Rio Grande", 
    id_sistema == 5 ~ "Rio Claro"
  ))
  
  return(dados_transferencia)
})


dados_coletados |> 
  bind_rows(df) |> 
  write_csv("resultados/transferencias.csv")
