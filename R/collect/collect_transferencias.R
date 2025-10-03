# Faz o download das transferências

# Carregar pacotes
library(glue)
library(dplyr)
library(writexl)

# datas ja baixadas: 

df <- read_csv("resultados/transferencias.csv")

datas_baixadas <- df |> 
  distinct(data) |> 
  pull() |> 
  as.Date()

# Aumentando o tempo de espera para 120 segundos
options(timeout = 600)

# Gerar datas
datas <- seq(as.Date("2010/01/01"),  Sys.Date(), 1) |> 
  as_tibble() |> 
  mutate(year = year(value), 
         month = month(value)) |> 
  group_by(year, month) |> 
  filter(value == max(value)) |> 
  select(value) |> 
  pull()

# datas faltantes

data_download <- as.Date(setdiff(datas, datas_baixadas))

data_download <- if (length(data_download) == 0) max(datas) else data_download


# Função para buscar dados e armazenar em data.frame
# Função para buscar dados e armazenar em data.frame
# Criar função resiliente com insistently()
safe_fromJSON <- insistently(fromJSON)

dados_coletados <- purrr::map_dfr(data_download, function(data) {
  
  
  data <- data_download[1]
  # Definir a URL do JSON
  url <- glue::glue("https://mananciais.sabesp.com.br/api/v4/boletins/mananciais/{data}")
  
  # Ler e converter o JSON com tentativa automática de repetição
  dados <- safe_fromJSON(url)
  
  # Extrair dados dos sistemas
  out <- dados[["data"]][["transferenciaData"]]
  
  # Extrair dados da ET
  return(dados_transferencia)
})


dados_coletados |> 
  distinct_all() |> 
  bind_rows(df) |> 
  write_csv("resultados/transferencias.csv")
