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
  select(-id_sistema) 

###### GERANDO OS ARQUIVOS E SALVANDO XLSX:
##### Cantareira: 

df_cantareira <- df_final |> 
  filter(`Nome do sistema` == "Cantareira") |> 
  mutate(across(11, ~ifelse(is.na(.), "", .))) |> 
  mutate(across(12, ~ifelse(is.na(.), 0, .))) |> 
  mutate(across(3:10, ~as.numeric(.)))
  

writexl::write_xlsx(df_cantareira, 
                    "resultados/vazao_transferencias/vazao_transferencia_cantareira.xlsx")

##### DF ALTO TIETE:

df_altotiete <- df_final |> 
  filter(`Nome do sistema` == "Alto Tietê") |> 
  pivot_wider(names_from = `Transferência`, 
              values_from = `Vazão de transferência`) |> 
  mutate(across(11:13, ~ifelse(is.na(.), 0, .))) |> 
  mutate(across(3:10, ~as.numeric(.)))

writexl::write_xlsx(df_altotiete, 
                    "resultados/vazao_transferencias/vazao_transferencia_alto_tiete.xlsx")


##### Guarapiranga

df_guarapiranga <- df_final |> 
  filter(`Nome do sistema` == "Guarapiranga") |> 
  pivot_wider(names_from = `Transferência`, 
              values_from = `Vazão de transferência`) |> 
  mutate(across(11:12, ~ifelse(is.na(.), 0, .))) |> 
  mutate(across(3:10, ~as.numeric(.)))

writexl::write_xlsx(df_guarapiranga, 
                    "resultados/vazao_transferencias/vazao_transferencia_guarapiranga.xlsx")



#### Rio Grande

df_riogrande <- df_final |> 
  filter(`Nome do sistema` == "Rio Grande") |> 
  mutate(across(11, ~ifelse(is.na(.), "", .))) |> 
  mutate(across(12, ~ifelse(is.na(.), 0, .))) |> 
  mutate(across(3:10, ~as.numeric(.)))

writexl::write_xlsx(df_guarapiranga, 
                    "resultados/vazao_transferencias/vazao_transferencia_rio_grande.xlsx")


