# Carregar pacotes
library(jsonlite)
library(glue)
library(tidyverse)


# Aumentando o tempo de espera para 120 segundos
options(timeout = 600)

# Gerar datas
datas <- seq(as.Date("2010/01/01"), as.Date("2025/03/31"), 1) |> 
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
  dados_sistemas <- data.frame(
    data = dados[["ReturnObj"]][["dadosSistemas"]][["Data"]][1],
    id_sistema = dados[["ReturnObj"]][["dadosSistemas"]][["SistemaId"]],
    volume = dados[["ReturnObj"]][["dadosSistemas"]][["VolumePorcentagem"]],
    variacao_volume = dados[["ReturnObj"]][["dadosSistemas"]][["VariacaoVolumePorcentagem"]],
    vazao_natural_diaria = dados[["ReturnObj"]][["dadosSistemas"]][["VazaoNatural"]],
    vazao_natural_mensal = dados[["ReturnObj"]][["dadosSistemas"]][["VazaoNaturalMediaNoMes"]],
    vazao_natural_media = dados[["ReturnObj"]][["dadosSistemas"]][["QMLTMensal"]],
    vazao_descarregada = dados[["ReturnObj"]][["dadosSistemas"]][["VazaoJusante"]],
    pluviometria = dados[["ReturnObj"]][["dadosSistemas"]][["Precipitacao"]],
    pluviometria_mensal = dados[["ReturnObj"]][["dadosSistemas"]][["PrecipitacaoAcumuladaNoMes"]],
    pluviometria_mensal_media = dados[["ReturnObj"]][["dadosSistemas"]][["PMLTMensal"]],
    #vazao_eta = dados[["ReturnObj"]][["dadosEtaDiario"]][["VazaoRetirada"]],
    vazao_media_eta = dados[["ReturnObj"]][["dadosSistemas"]][["VazaoRetiradaMediaNoMes"]]
  ) |> 
    mutate(Sistema = case_match(id_sistema, 
                                0 ~ "Cantareira", 
                                1 ~ "Alto Tietê", 
                                2 ~ "Guarapiranga", 
                                3 ~ "Alto Cotia", 
                                4 ~ "Rio Grande", 
                                5 ~ "Rio Claro", 
                                17 ~ "São Lourenço", 
                                19 ~ "Cantareira Velho", 
                                99 ~ "SIM"), .after = id_sistema)
  
  # Extrair dados da ETA
  dados_eta <- data.frame(
    data = dados[["ReturnObj"]][["dadosSistemas"]][["Data"]][1],
    id_sistema = dados[["ReturnObj"]][["dadosEtaDiario"]][["SistemaId"]], 
    id_componente = dados[["ReturnObj"]][["dadosEtaDiario"]][["ComponenteId"]],
    vazao_captada = dados[["ReturnObj"]][["dadosEtaDiario"]][["VazaoRetirada"]]
  ) |> 
    mutate(Sistema = case_when(
      id_sistema == 0 ~ "Cantareira", 
      id_sistema == 1 ~ "Alto Tietê", 
      id_sistema == 2 ~ "Guarapiranga", 
      id_sistema == 3 & id_componente == 27 ~ "Alto Cotia", 
      id_sistema == 3 & id_componente != 25 ~ "Baixo Cotia", 
      id_sistema == 4 & id_componente == 25 ~ "Rio Grande", 
      id_sistema == 4 & id_componente != 25 ~ "Ribeirão da Estiva", 
      id_sistema == 5 ~ "Rio Claro", 
      id_sistema == 17 ~ "São Lourenço", 
      id_sistema == 19 ~ "Cantareira Velho", 
      id_sistema == 99 ~ "SIM"
    )) |> 
    group_by(data, Sistema) |> 
    summarise(vazao_captada = sum(vazao_captada)) # resolve cantareira velho
    
    vazao_eta_sim <- dados_eta |> 
      filter(Sistema != "Cantareira Velho") |> 
      ungroup() |> 
      summarise(sum(vazao_captada)) |> 
      pull()
  
  # Realizar o full_join entre os dois bancos de dados (usando a coluna id_sistema)
  dados_completos <- full_join(dados_sistemas, dados_eta, by = c("Sistema", "data")) |> 
    mutate(vazao_captada = ifelse(Sistema == "SIM", 
                                  vazao_eta_sim, vazao_captada))
  
  return(dados_completos)
})

  ### Ajustando: 

tbl <- dados_coletados |> 
  mutate(vazao_media_eta = 
           ifelse(Sistema %in% c("Baixo Cotia", "Ribeirão da Estiva"), 
         vazao_captada, vazao_media_eta))

# Salvar como CSV
write_csv2(tbl, "dados_mananciais_completo.csv")

# Ajustando os dados

aux <- tbl |> 
  mutate(Data = floor_date(ymd_hms(data), "day")) |> 
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

write_csv2(aux_selecionado, "resultados/dados_mananciais_ajustados.csv")


#### Lendo

df <- read_csv2("resultados/dados_mananciais_ajustados.csv") |> 
  mutate(across(
    -c(Data, `Nome do sistema`),
    \(x) str_replace(x,'\\.',',')
  ))


writexl::write_xlsx(df, "resultados/dados_mananciais_adequados.xlsx")
