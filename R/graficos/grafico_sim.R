library(tidyverse)
library(lubridate)
library(readxl)
library(ggrepel)
library(patchwork) # Para combinar os gráficos
library(ggh4x)

# Carrega e pré-processa
df <- readxl::read_excel("resultados/vazao_transferencias/vazao_SIM.xlsx")

df <- df |> 
  dplyr::mutate(across(3:ncol(df), ~ gsub(",", ".", as.character(.)))) |> 
  dplyr::mutate(across(3:ncol(df), as.numeric)) |> 
  dplyr::mutate(Data = ymd(Data),
         year = year(Data),
         mes = month(Data))

# v1 = volume diario; # presente
# v2 = vazao natural mensal;  # presente
# v3 = vazao natural mensal/media historica; # presente
# v4 = pluviometria mensal;  # ausente
# v5 = pluviometria mensal / media historica; # ausente
# v6 = vazao captada eta # presente

df_sel <- df |> 
  dplyr::select(Data, year, mes, 
                "Volume Diário (%)" = `Volume Diária (%)`,
                `Vazão natural mensal (m³/s)`,
                `Vazão natural Mensal / Média histórica (%)`,
                `Vazão captada da ETA (m³/s)`,
                -`Nome do sistema`)


# Anos mais recentes
ultimos_anos <- df_sel |> 
  dplyr::distinct(year) |> 
  dplyr::arrange(desc(year)) |> 
  dplyr::slice(1:5) |> 
  dplyr::pull(year)

# paleta
azuis <- c(
  "#cce5ff",  # azul bem claro
  "#99ccff",  # azul claro
  "#6699ff",  # azul médio
  "#3366cc",  # azul escuro
  "#003366"   # azul bem escuro
)



df_sel2 <- df_sel |> 
  tidyr::pivot_longer(!c(Data, year, mes),
                      names_to = "var", values_to = "valores") |> 
  dplyr::mutate(alpha = case_when(year %in% ultimos_anos ~ 1,
                                  TRUE ~ 0.85),
                thick = case_when(year %in% ultimos_anos ~ 1.5,
                                  TRUE ~ 1),
                colourz = case_when(
                  year == 2021 ~ "#cce5ff",
                  year == 2022 ~ "#99ccff",
                  year == 2023 ~ "#6699ff",
                  year == 2024 ~ "#3366cc",
                  year == 2025 ~ "#003366",
                  TRUE ~ "#d9d9d9")) |> 
  dplyr::mutate(
    colourz = as.character(colourz),  
    alpha = as.numeric(alpha))

#labels finais
labels_finais <- df_sel2 |> 
  dplyr::filter((year %in% c(2021, 2022, 2023, 2024) & mes == 12) |
                  (year == 2025 & mes == 3))

cores_anos <- df_sel2 |>
  dplyr::distinct(year, colourz) |>
  dplyr::arrange(year) |>
  dplyr::mutate(year = ifelse(colourz == "#d9d9d9", "Anterior", year)) |> 
  deframe()


plot_sim <- df_sel2 |> 
  ggplot(aes(x = mes, y = valores, group = year)) +
  geom_line(aes(alpha = alpha, color = colourz, size = thick)) +
  geom_text(data = labels_finais,
            aes(y = valores, label = year),
            size = 3, color = "black", hjust = 0) +
  facet_wrap(~var, ncol = 2, scales = "free_y") +
  scale_color_identity(
    guide = "legend",
    labels = names(cores_anos),
    breaks = cores_anos
  ) +
  guides(color = guide_legend(nrow = 1)) + 
  scale_alpha_identity() +
  scale_size_identity() +
  scale_x_continuous(breaks = 1:12, limits = c(1, 13)) +
  labs(x = "Meses", color = "Ano", y = "Valores", 
       title = "Sistema SIM",
       caption = "Valores reais.") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "#fdf6e3", color = NA),  # fundo dos painéis (tom pastel claro)
    plot.background = element_rect(fill = "#fdf6e3", color = NA),   # fundo geral do gráfico
    strip.background = element_rect(fill = "#eee8d5", color = NA),  # fundo das faixas (facets)
    panel.grid.minor = element_line(color = "#dcdcdc", linetype = "dotted", size = 0.3),  # grade menor
    panel.grid.major = element_line(color = "#cccccc", linetype = "solid", size = 0.4),   # grade maior (opcionalmente suavizada também)
    legend.background = element_rect(fill = "#fdf6e3", color = NA),  # fundo da legenda
    legend.key = element_rect(fill = "#fdf6e3", color = NA)          # fundo dos elementos da legenda
  )

plot_sim
ggsave(
  filename = "plot_sim_jul25.png",
  plot = plot_sim,
  width = 13,    
  height = 13,   
  dpi = 400      
)
