### Este código le os dados coletados e transforma nas transferências que 
## são utilizadas para consolidar os bancos de dados

#### Library

require(tidyverse)


#### dados

sistemas <- read_csv2("resultados/dados_mananciais_ajustados.csv")

transferencias <- read_csv("resultados/transferencias.csv") |> 
  rename(`Nome do sistema` = Sistema) |> 
  rename(Data = data)



### Juntando: 

df_final <- sistemas |> 
  left_join(transferencias) |> 
  rename(`Transferência` = Abreviatura, 
         `Vazão de transferência` = Valor) |> 
  select(-id_sistema) |> 
  rename(
    `Vazão natural mensal (m³/s)` = `Vazão natural Mensal`,
    `Vazão Média Histórica Mensal (m³/s)` = `Vazão Média Histórica Mensal`,
    `Vazão natural Mensal / Média histórica (%)` = `Vazão natural Mensal / Média histórica`,
    `Pluviometria Média Histórica Mensal (mm)` = `Pl. Média Histórica Mensal`,
    `Pluviometria mensal/média histórica (%)` = `Chuva mensal/média histórica`, 
    `Vazão captada da ETA (m³/s)` = `Vazão captada da ETA`, 
    `Vazão de transferência (m³/s)` = `Vazão de transferência`
  ) |> 
  mutate(across(
    -c(Data, `Nome do sistema`, `Transferência`),
    \(x) str_replace(x,'\\.',',')
  ))

###### GERANDO OS ARQUIVOS E SALVANDO XLSX:
##### Cantareira: 

df_cantareira <- df_final |> 
  filter(`Nome do sistema` == "Cantareira") |> 
  mutate(across(11, ~ifelse(is.na(.), " ", .))) |> 
  mutate(across(12, ~ifelse(is.na(.), 0, .))) |> 
  pivot_wider(names_from = `Transferência`, 
              values_from = `Vazão de transferência (m³/s)`, 
              values_fill = "0") |> 
  select(-c(11))
  

writexl::write_xlsx(df_cantareira, 
                    "resultados/vazao_transferencias/vazao_transferencia_cantareira.xlsx")

##### DF ALTO TIETE:

df_altotiete <- df_final |> 
  filter(`Nome do sistema` == "Alto Tietê") |> 
  pivot_wider(names_from = `Transferência`, 
              values_from = `Vazão de transferência (m³/s)`, 
              values_fill = "0") |> 
  mutate(across(11:13, ~ifelse(is.na(.), 0, .)))

writexl::write_xlsx(df_altotiete, 
                    "resultados/vazao_transferencias/vazao_transferencia_alto_tiete.xlsx")


##### Guarapiranga

df_guarapiranga <- df_final |> 
  filter(`Nome do sistema` == "Guarapiranga") |> 
  pivot_wider(names_from = `Transferência`, 
              values_from = `Vazão de transferência (m³/s)`, 
              values_fill = "0") |> 
  mutate(across(11:12, ~ifelse(is.na(.), 0, .)))

writexl::write_xlsx(df_guarapiranga, 
                    "resultados/vazao_transferencias/vazao_transferencia_guarapiranga.xlsx")


#### Rio Grande

df_riogrande <- df_final |> 
  filter(`Nome do sistema` == "Rio Grande") |> 
  mutate(across(11, ~ifelse(is.na(.), "", .))) |> 
  mutate(across(12, ~ifelse(is.na(.), 0, .)))

writexl::write_xlsx(df_riogrande, 
                    "resultados/vazao_transferencias/vazao_transferencia_rio_grande.xlsx")


#### RIO CLARO

df_rioclaro <- df_final |> 
  filter(`Nome do sistema` == "Rio Claro") |> 
  mutate(across(11, ~ifelse(is.na(.), "", .))) |> 
  mutate(across(12, ~ifelse(is.na(.), 0, .)))

writexl::write_xlsx(df_rioclaro, 
                    "resultados/vazao_transferencias/vazao_transferencia_rio_claro.xlsx")


#### Alto cotia: 

df_alto_cotia <- df_final |> 
  filter(`Nome do sistema` == "Alto Cotia") |> 
  mutate(across(11, ~ifelse(is.na(.), "", .))) |> 
  mutate(across(12, ~ifelse(is.na(.), 0, .))) |> 
  select(-c(11,12))

writexl::write_xlsx(df_alto_cotia, 
                    "resultados/vazao_transferencias/vazao_alto_cotia.xlsx")



### Sao lourenco

df_sao_lourenco <- df_final |> 
  filter(`Nome do sistema` == "São Lourenço") |> 
  mutate(across(11, ~ifelse(is.na(.), "", .))) |> 
  select(-c(11,12))


writexl::write_xlsx(df_sao_lourenco, 
                    "resultados/vazao_transferencias/vazao_sao_lourenco.xlsx")

#### SIM

df_sim <- df_final |> 
  filter(`Nome do sistema` == "SIM") |> 
  mutate(across(11, ~ifelse(is.na(.), "", .))) |> 
  select(c(1:6,10))

writexl::write_xlsx(df_sim, 
                    "resultados/vazao_transferencias/vazao_SIM.xlsx")


### Ribeirao

df_ribeirao <- df_final |> 
  filter(`Nome do sistema` == "Ribeirão da Estiva") |> 
  mutate(across(11, ~ifelse(is.na(.), "", .))) |> 
  select(-c(11,12))


writexl::write_xlsx(df_ribeirao, 
                    "resultados/vazao_transferencias/vazao_ribeirao.xlsx")


### Baixo Cotia

df_baixo_cotia <- df_final |> 
  filter(`Nome do sistema` == "Baixo Cotia") |> 
  mutate(across(11, ~ifelse(is.na(.), "", .))) |> 
  select(-c(11,12))

writexl::write_xlsx(df_baixo_cotia, 
                    "resultados/vazao_transferencias/vazao_baixo_cotia.xlsx")

