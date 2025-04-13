library(tidyverse)
library(lubridate)
library(readxl)
library(ggrepel)
library(patchwork) # Para combinar os gráficos

# Carrega e pré-processa
df <- readxl::read_xlsx("resultados/vazao_transferencias/vazao_transferencia_cantareira.xlsx") |> 
  mutate(across(3:11, ~ gsub(",", ".", as.character(.)))) |> 
  mutate(across(3:11, as.numeric)) |> 
  mutate(Data = ymd(Data),
         year = year(Data),
         mes = month(Data))

# Lista de variáveis a serem plotadas
vars <- names(df)[3:10]

vars <- vars[-c(3,6)]

# Variáveis que NÃO devem ser normalizadas
vars_sem_scale <- c("Vazão natural Mensal / Média histórica (%)", 
                    "Pluviometria mensal/média histórica (%)")

# Função geradora de gráfico
gera_grafico <- function(var_nome) {
  df_sel <- df |> 
    select(Data, year, mes, all_of(var_nome)) |> 
    rename(valor = all_of(var_nome))
  
  # Aplica scale se necessário
  if (!(var_nome %in% vars_sem_scale)) {
    df_sel <- df_sel |> 
      group_by(year) |> 
      mutate(valor_z = scale(valor)[,1]) |> 
      ungroup()
    eixo_y <- "Z-score"
    linha_media <- 0
  } else {
    df_sel <- df_sel |> 
      mutate(valor_z = valor)
    eixo_y <- var_nome
    linha_media <- 1  # Média histórica = 100%
  }
  
  # Anos mais recentes
  ultimos_anos <- df_sel |> 
    distinct(year) |> 
    arrange(desc(year)) |> 
    slice(1:5) |> 
    pull(year)
  
  # Ponto final para rótulo
  labels_finais <- df_sel |> 
    filter(year %in% ultimos_anos, mes == 12) |> 
    group_by(year) |> 
    slice_tail(n = 1)
  
  ggplot(df_sel, aes(x = mes, y = valor_z, group = year)) +
    geom_line(data = df_sel |> filter(!year %in% ultimos_anos),
              color = "gray80", size = 0.4) +
    geom_line(data = df_sel |> filter(year %in% ultimos_anos),
              aes(color = factor(year)), size = 1.2) +
    geom_text_repel(data = labels_finais,
                    aes(label = year, color = factor(year)),
                    nudge_x = 0.5, direction = "y", hjust = 0,
                    segment.color = "transparent", size = 3.5, show.legend = FALSE) +
    geom_hline(yintercept = linha_media, color = "black") +
    scale_x_continuous(breaks = 1:12) +
    scale_color_brewer(palette = "Reds") +
    labs(title = var_nome,
         x = "Meses", y = eixo_y, color = "Ano") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "none")
}

# Gera todos os gráficos
lista_graficos <- map(vars, gera_grafico)

# Mostra os gráficos juntos
wrap_plots(lista_graficos, ncol = 2) +
  plot_annotation(title = "Comparação mensal por variável",
                  subtitle = "Com destaque para os 5 anos mais recentes")