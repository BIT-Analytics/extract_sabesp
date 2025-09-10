library(tidyverse)
library(lubridate)
library(readxl)
library(ggrepel)
library(patchwork) # Para combinar os gráficos
library(ggh4x)
library(changepoint)

# Padronizacao de data e idioma
# MAC/Linux
Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

# Windows
# Sys.setlocale("LC_TIME", "Portuguese_Brazil.1252")



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
  labs(x = "Meses", color = "Ano", y = "Valores", title = "Cantareira",
       caption = "Vazão Transferência Cantareira - Volume Diária (%), Vazão natural mensal (m³/s), Pluviometria Mensal (mm) e Vazão captada da ETA (m³/s) com valores reais.") +
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
  filename = "cantareira_semnormalizar_jul25.png",
  plot = plot1,
  width = 10,    
  height = 14,   
  dpi = 400      
)



plot2x <- df_sel3 |> 
  dplyr::filter(var == "Vazão captada da ETA (m³/s)") |> 
  dplyr::mutate(var = fct_drop(var)) |>
  tibble::rowid_to_column() |> 
  ungroup() |> 
  dplyr::mutate(data_aj = as.Date(Data))

plot2 <- plot2x |> 
  ggplot(aes(x = data_aj)) +
  geom_ribbon(aes(ymin = 0, ymax = 40, fill = colourz),
              alpha = 0.7) +
  geom_line(aes(y = valores),
            color = "black", alpha = 1, size = 0.7) +  
  scale_fill_identity(  # Adiciona parâmetros de legenda para fill
    guide = "legend",
    labels = names(cores_anos),
    breaks = cores_anos
  ) +
  guides(fill = guide_legend(nrow = 1)) +  # Controla a legenda de fill
  labs(
    x = "Período", 
    fill = "Ano",  
    y = "Valores", 
    caption = "Vazão captada da ETA (m³/s) com valores reais.", 
    title = "Cantareira - Vazão captada da ETA (m³/s)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "#fdf6e3", color = NA),
    plot.background = element_rect(fill = "#fdf6e3", color = NA),
    strip.background = element_rect(fill = "#eee8d5", color = NA),
    panel.grid.minor = element_line(color = "#dcdcdc", linetype = "dotted", size = 0.3),
    panel.grid.major = element_line(color = "#cccccc", linetype = "solid", size = 0.4),
    legend.background = element_rect(fill = "#fdf6e3", color = NA),
    legend.key = element_rect(fill = "#fdf6e3", color = NA)
  )

plot2

ggsave(
  filename = "plot2_vazaocaptada_semnormalizar_jul25.png",
  plot = plot2,
  width = 13,    
  height = 9,   
  dpi = 400      
)

####
####
pelt_serie <- cpt.mean(data = plot2x$valores, method = "PELT") 
binseg_serie <- cpt.mean(data = plot2x$valores, method = "BinSeg")

plot(pelt_serie)

plot(binseg_serie, cpt.width = 3, cpt.col = 'blue')
cpts(binseg_serie)


# otimizando
Q <- floor((length(plot2x$valores)/2)) + 1
Penalidade <- "SIC"
binseg_serie <- cpt.mean(data = plot2x$valores, method = "BinSeg",
                         Q = Q, penalty = Penalidade)
plot(binseg_serie)

# numero de pontos
plot_n_segmentos <-binseg_serie@pen.value.full |> 
  data.frame()
colnames(plot_n_segmentos) <- "SIC"

plot1 <- plot_n_segmentos |> 
  dplyr::mutate(Pontos = (1:nrow(plot_n_segmentos))) |> 
  ggplot2::ggplot() +
  geom_path(aes(x = Pontos, 
                y = SIC)) +
  geom_point(aes(x = Pontos,
                 y = SIC,
                 fill = SIC),
             size = 4, shape = 21, show.legend = F) +
  scale_fill_gradient2(low = "red", high = "blue")
plot1

# reduzindo para ate 4 pontos/segmentos
QQ <- 3
binseg_serie2 <- cpt.mean(data = plot2x$valores, method = "BinSeg",
                          Q = QQ, penalty = Penalidade)
plot(binseg_serie2)

# df com a série em análise
mv1 <- data.frame(ts = plot2x$valores,
                  data = plot2x$Data)

# Identificar changepoints (ajuste o método conforme necessário)
binseg_serie2 <- changepoint::cpt.mean(mv1$ts, method = "BinSeg", Q = QQ) 
md <- changepoint::param.est(binseg_serie2)[[1]]  # Médias por segmento
cpts <- changepoint::cpts(binseg_serie2)  # Índices dos changepoints

# Converter índices dos changepoints para datas correspondentes
cpt_dates <- mv1$data[cpts]

# plots
segments_data <- data.frame(
  x = c(min(plot2x$Data), cpt_dates),
  xend = c(cpt_dates, max(plot2x$Data)),
  y = md[1:(QQ+1)],
  yend = md[1:(QQ+1)],
  segment_type = "Changepoint"
)

# 
p2 <- mv1 |> 
  ggplot(aes(x = data, y = ts)) +
  geom_line() +
  geom_segment(
    data = segments_data,
    aes(x = x, y = y, xend = xend, yend = yend, color = segment_type),
    size = 1
  ) +
  scale_color_manual(values = "blue", name = NULL) +
  scale_x_date(
    date_breaks = "4 month",
    date_labels = "%b %Y",     # Formato da data "Jan 2025"
    expand = c(0, 0)           # Remove espaços extras nos extremos
  ) +
  labs(
    x = "Período", 
    y = "Valores", 
    caption = "Changepoint (segmentação binária) de vazão captada da ETA (m³/s) com valores reais.", 
    title = "Cantareira - Vazão captada da ETA (m³/s)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "#fdf6e3", color = NA),
    plot.background = element_rect(fill = "#fdf6e3", color = NA),
    strip.background = element_rect(fill = "#eee8d5", color = NA),
    panel.grid.minor = element_line(color = "#dcdcdc", linetype = "dotted", size = 0.3),
    panel.grid.major = element_line(color = "#cccccc", linetype = "solid", size = 0.4),
    legend.background = element_rect(fill = "#fdf6e3", color = NA),
    legend.key = element_rect(fill = "#fdf6e3", color = NA), 
    axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1,
                               size = 8)
  )
p2


ggsave(
  filename = "changept_vazaocaptada_cantar_jul25.png",
  plot = p2,
  width = 13,    
  height = 9,   
  dpi = 400      
)
