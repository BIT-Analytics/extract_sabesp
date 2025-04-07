### Library

require(tidyverse)


### Carregando arquivo: 

df <- readxl::read_xlsx("resultados/vazao_transferencias/vazao_transferencia_cantareira.xlsx") |>
  mutate(across(3:11, ~ gsub(",", ".", as.character(.)))) |> 
  mutate(across(3:11, ~as.numeric(.))) |> 
  mutate(Data = ymd(Data)) |> 
  mutate(year = year(Data), 
         mes = month(Data))

# Calcular a média anual e geral
media_ano <- df |> 
  group_by(year) |> 
  summarise(media_anual = mean(`Volume Diária (%)`, na.rm = TRUE))

media_geral <- mean(media_ano$media_anual, na.rm = TRUE)

# Preciso pensar em como faremos isto aqui, a principio me parece desnecessário:
media_ano <- media_ano |> 
  mutate(classe = case_when(
    media_anual > media_geral ~ "acima",
    media_anual < media_geral ~ "abaixo",
    TRUE ~ "neutro"
  ))

# Juntar com dados principais:
df_plot <- df |> left_join(media_ano, by = "year")

# Identificar os anos destacados (Como serão destacados? botei 5 ultimos)
anos_destaque <- media_ano |> 
  arrange(desc(year)) |> 
  slice_head(n = 5) |> 
  pull(year)

# Gráfico
df_plot |>  
  ggplot(aes(x = mes, y = `Volume Diária (%)`, group = year)) +
  geom_line(data = df_plot |> filter(!(year %in% anos_destaque)),
            color = "grey80", size = 0.3) +
  geom_line(data = df_plot |> filter(year %in% anos_destaque & classe == "acima"),
            aes(color = as.factor(year)), size = 1) +
  geom_line(data = df_plot |> filter(year %in% anos_destaque & classe == "abaixo"),
            aes(color = as.factor(year)), size = 1) +
  geom_hline(yintercept = media_geral, color = "black", size = 0.5) +
  geom_text(data = df_plot |> 
              filter(year %in% anos_destaque) |> 
              group_by(year) |> 
              slice_tail(n = 1),
            aes(label = year), hjust = -0.1, vjust = 0.5, size = 3) +
  scale_color_manual(values = c("blue", "red", "darkred", "firebrick", "darkblue")) +  # ajuste conforme os anos
  scale_x_continuous(breaks = 1:12) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs(x = "Meses", y = "Volume Diário (%)",
       title = "Anos em comparação à média geral",
       subtitle = paste("Linha preta: média das médias anuais =", round(media_geral, 1), "%"),
       caption = "Fonte: Dados SABESP")
