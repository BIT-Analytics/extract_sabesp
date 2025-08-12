library(tidyverse)
library(lubridate)
library(readxl)
library(ggrepel)
library(patchwork) # Para combinar os gráficos
library(ggh4x)

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

df_sel <- df |> 
  dplyr::select(Data, year, mes, all_of(vars)) |> 
  dplyr::rename(valor = all_of(vars))

# v1 = volume diario; v2 = vazao natural mensal; v3 = vazao natural mensal/media historica;
# v4 = pluviometria mensal; v5 = pluviometria mensal / media historica; v6 = vazao captada eta

# tudo sem padronizacao zscore
df_sel2 <- df_sel |> 
  dplyr::group_by(year) |> 
  dplyr::mutate( # if
    valor_z1 = valor1,
    valor_z2 = valor2,
    valor_z4 = valor4,
    valor_z6 = valor6,
    
    # else
    valor_z3 = valor3,
    valor_z5 = valor5
    
  ) |> 
  ungroup()



# Anos mais recentes
ultimos_anos <- df_sel2 |> 
  distinct(year) |> 
  arrange(desc(year)) |> 
  slice(1:5) |> 
  pull(year)

# paleta
azuis <- c(
  "#cce5ff",  # azul bem claro
  "#99ccff",  # azul claro
  "#6699ff",  # azul médio
  "#3366cc",  # azul escuro
  "#003366"   # azul bem escuro
)


# ajuste df
df_sel3 <- df_sel2 |> 
  dplyr::select(-c(valor1, valor2, valor3, valor4, valor5, valor6)) |> 
  dplyr::rename("Volume Diário (%)" = "valor_z1",
                "Vazão natural mensal (m³/s)" = "valor_z2",
                "Vazão natural Mensal / Média histórica (%)" = "valor_z3",
                "Pluviometria Mensal (mm)" = "valor_z4",
                "Pluviometria mensal / média histórica (%)" = "valor_z5",
                "Vazão captada da ETA (m³/s)" = "valor_z6") |> 
  tidyr::pivot_longer(!c(Data, year, mes),
                      names_to = "var", values_to = "valores") |> 
  dplyr::mutate(linha_media = ifelse(var == "Pluviometria mensal / média histórica (%)" |
                                       var == "Vazão natural Mensal / Média histórica (%)",
                                     1, 0),
                eixo_y = ifelse(linha_media == 1, var, "Z-score"),
                alpha = case_when(year %in% ultimos_anos ~ 1,
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
    alpha = as.numeric(alpha),
    var = fct_relevel(
      var,
      "Pluviometria mensal / média histórica (%)",
      "Vazão natural Mensal / Média histórica (%)"
    ),
    yintercept = ifelse(linha_media == 1, 0, 1),  
    ylim_min = ifelse(linha_media == 1, -4, -3),  
    ylim_max = ifelse(linha_media == 1, 4, 3)
  )


# ultimos anos
labels_finais <- df_sel3 |> 
  dplyr::filter((year %in% c(2021, 2022, 2023, 2024) & mes == 12) |
                  (year == 2025 & mes == 3))



# cores plot
cores_anos <- df_sel3 |>
  dplyr::distinct(year, colourz) |>
  dplyr::arrange(year) |>
  dplyr::mutate(year = ifelse(colourz == "#d9d9d9", "Anterior", year)) |> 
  deframe()

# plot

plot1 <- df_sel3 |> 
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
       caption = "Volume Diária (%), Vazão natural mensal (m³/s), Pluviometria Mensal (mm) e Vazão captada da ETA (m³/s) com valores reais.") +
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

plot1

ggsave(
  filename = "plot2_facetado_semnormalizar.png",
  plot = plot1,
  width = 10,    
  height = 14,   
  dpi = 400      
)



plot2 <- df_sel3 |> 
  dplyr::filter(var == "Vazão captada da ETA (m³/s)") |> 
  dplyr::mutate(var = fct_drop(var)) |>
  tibble::rowid_to_column() |> 
  ungroup() |> 
  ggplot(aes(x = rowid, y = valores)) +
  geom_line(alpha = 1, size = 1.2, color = "black") +
  geom_line(aes(color = colourz), alpha = 1, size = 1.2) +
  scale_color_identity(
    guide = "legend",
    labels = names(cores_anos),
    breaks = cores_anos
  ) +
  guides(color = guide_legend(nrow = 1)) + 
  labs(x = "Período", color = "Ano", y = "Valores", 
       caption = "Vazão captada da ETA (m³/s) com valores reais.", 
       title = "Vazão captada da ETA (m³/s)") +
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

plot2

ggsave(
  filename = "plot2_vazaocaptada_semnormalizar.png",
  plot = plot2,
  width = 13,    
  height = 9,   
  dpi = 400      
)
