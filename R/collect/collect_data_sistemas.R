# Faz o download dos dados na tabela presente no site da SABESP

# Carregar pacotes
library(jsonlite)
library(glue)
library(dplyr)
library(writexl)
# library(tidyverse)
# datas ja baixadas: 

df <- read_csv2("dados_mananciais_completo.csv")

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

# Ele sempre vai rodar, bom para saber se funciona
# Mais a frente é feito um distinct all

data_download <- if (length(data_download) == 0) max(datas) else data_download


# Função para buscar dados e armazenar em data.frame
# Função para buscar dados e armazenar em data.frame
dados_coletados <- purrr::map_dfr(data_download, function(data) {
  
  # Definir a URL do JSON
  #url <- glue("https://mananciais-sabesp.fcth.br/api/Mananciais/Boletins/Mananciais/{data}")
  
  url2 <- glue("https://mananciais.sabesp.com.br/api/v4/boletins/mananciais/{data}")
  # Ler e converter o JSON
  dados <- fromJSON(url2)
  
  # Extrair dados dos sistemas
    dados_sistemas <- data.frame(
      data                   = dados[["data"]][["sistemasData"]][["date"]],
      id_sistema             = dados[["data"]][["sistemasData"]][["idSistema"]],
      volume                 = dados[["data"]][["sistemasData"]][["volumeUtilArmazenadoPorcentagem"]],
      variacao_volume        = dados[["data"]][["sistemasData"]][["variacaoVolumeUtil"]],
      vazao_natural_diaria   = dados[["data"]][["sistemasData"]][["vazaoNatural"]],
      vazao_natural_mensal   = dados[["data"]][["sistemasData"]][["vazaoNaturalNoMes"]],
      vazao_natural_media    = dados[["data"]][["sistemasData"]][["vazaoNaturalMediaHistorica"]],
      vazao_descarregada     = dados[["data"]][["sistemasData"]][["vazaoJusante"]],
      pluviometria           = dados[["data"]][["sistemasData"]][["chuva"]],
      pluviometria_mensal    = dados[["data"]][["sistemasData"]][["chuvaAcumuladaNoMes"]],
      pluviometria_mensal_media = dados[["data"]][["sistemasData"]][["chuvaMediaHistorica"]],
      vazao_media_eta        = dados[["data"]][["sistemasData"]][["vazaoRetiradaNoMes"]]
    ) |>
      mutate(
        Sistema = case_match(
          id_sistema,
          0  ~ "Cantareira",
          1  ~ "Alto Tietê",
          2  ~ "Guarapiranga",
          3  ~ "Alto Cotia",
          4  ~ "Rio Grande",
          5  ~ "Rio Claro",
          17 ~ "São Lourenço",
          19 ~ "Cantareira Velho",
          99 ~ "SIM"
        ),
        .after = id_sistema
      ) |> 
    mutate(Sistema = case_match(id_sistema, 
                                64 ~ "Cantareira", 
                                65 ~ "Alto Tietê", 
                                66 ~ "Guarapiranga", 
                                67 ~ "Alto Cotia", 
                                68 ~ "Rio Grande", 
                                69 ~ "Rio Claro", 
                                72 ~ "São Lourenço", 
                                74 ~ "Cantareira Velho", 
                                75 ~ "SIM"), .after = id_sistema)
  
  # Extrair dados da ETA
  # dados_eta <- data.frame(
  #   data = dados[["data"]][["sistemasData"]][["dadosSistemas"]][["Data"]][1],
  #   id_sistema = dados[["data"]][["sistemasData"]][["dadosEtaDiario"]][["SistemaId"]], 
  #   id_componente = dados[["data"]][["sistemasData"]][["dadosEtaDiario"]][["ComponenteId"]],
  #   vazao_captada = dados[["data"]][["sistemasData"]][["dadosEtaDiario"]][["VazaoRetirada"]]
  # ) |> 
  #   mutate(Sistema = case_when(
  #     id_sistema == 64 ~ "Cantareira", 
  #     id_sistema == 65 ~ "Alto Tietê", 
  #     id_sistema == 66 ~ "Guarapiranga", 
  #     id_sistema == 3 & id_componente == 27 ~ "Alto Cotia", 
  #     id_sistema == 3 & id_componente != 25 ~ "Baixo Cotia", 
  #     id_sistema == 4 & id_componente == 25 ~ "Rio Grande", 
  #     id_sistema == 4 & id_componente != 25 ~ "Ribeirão da Estiva", 
  #     id_sistema == 5 ~ "Rio Claro", 
  #     id_sistema == 17 ~ "São Lourenço", 
  #     id_sistema == 19 ~ "Cantareira Velho", 
  #     id_sistema == 99 ~ "SIM"
  #   )) |> 
  #   group_by(data, Sistema) |> 
  #   summarise(vazao_captada = sum(vazao_captada)) # resolve cantareira velho
  #   
  #   vazao_eta_sim <- dados_eta |> 
  #     filter(Sistema != "Cantareira Velho") |> 
  #     ungroup() |> 
  #     summarise(sum(vazao_captada)) |> 
  #     pull()
  # 
  # # Realizar o full_join entre os dois bancos de dados (usando a coluna id_sistema)
  # dados_completos <- full_join(dados_sistemas, dados_eta, by = c("Sistema", "data")) |> 
  #   mutate(vazao_captada = ifelse(Sistema == "SIM", 
  #                                 vazao_eta_sim, vazao_captada))
  
  return(dados_sistemas)
})

  ### Ajustando: 

tbl <- dados_coletados |> 
  # mutate(vazao_media_eta = 
  #          ifelse(Sistema %in% c("Baixo Cotia", "Ribeirão da Estiva"), 
  #        vazao_captada, vazao_media_eta)) |> 
  mutate(data = as.Date(data)) |> 
  bind_rows(df) |> 
  distinct_all()

# Salvar como CSV
write_csv2(tbl, "dados_mananciais_completo.csv")

# Ajustando os dados

aux <- tbl |> 
  mutate(Data = floor_date(ymd(data), "day")) |> 
  mutate(`Vazão natural Mensal / Média histórica` = vazao_natural_mensal / vazao_natural_media) |> 
  mutate(`Chuva mensal/média histórica` = pluviometria_mensal / pluviometria_mensal_media) |> 
  rename(`Nome do sistema` = Sistema, 
         `Volume Diária (%)` = volume, 
         `Vazão natural Mensal` = vazao_natural_mensal, 
         `Vazão Média Histórica Mensal` = vazao_natural_media, 
         `Pluviometria Mensal (mm)` = pluviometria_mensal, 
         `Pl. Média Histórica Mensal` = pluviometria_mensal_media, 
         `Vazão captada da ETA` = vazao_captada)

### Selecionar na ordem pedida por Eduardo: 

aux_selecionado <- aux[, c("Data", 
                           "Nome do sistema", 
                           "Volume Diária (%)", 
                           "Vazão natural Mensal", 
                           "Vazão Média Histórica Mensal", 
                           "Vazão natural Mensal / Média histórica", 
                           "Pluviometria Mensal (mm)", 
                           "Pl. Média Histórica Mensal", 
                           "Chuva mensal/média histórica", 
                           "Vazão captada da ETA")]

aux_selecionado <- aux_selecionado %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

aux_selecionado[is.na(aux_selecionado)] <- "-"

aux_selecionado <- aux_selecionado %>%
  mutate(
    Data = as.Date(Data),
    ano_mes = floor_date(Data, "month")   # cria grupos por ano+mês
  ) %>%
  group_by(ano_mes) %>%
  filter(Data == max(Data)) %>%           # pega o último dia de cada mês/ano
  ungroup()


write_csv2(aux_selecionado, "resultados/dados_mananciais_ajustados.csv")


#### Lendo

df <- read_csv2("resultados/dados_mananciais_ajustados.csv") |> 
  mutate(across(
    -c(Data, `Nome do sistema`),
    \(x) str_replace(x,'\\.',',')))


writexl::write_xlsx(df, "resultados/dados_mananciais_adequados.xlsx")
