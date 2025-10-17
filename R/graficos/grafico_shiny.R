library(tidyverse)
library(echarts4r)

df <- read_csv2("dados_mananciais_completo.csv")



# volume util ----
# esse grafico deve ser por percentual no eixo Y
df |>
  filter(Sistema == "Cantareira") |>
  filter(data >= as.Date("2025-01-01") & data <= as.Date("2025-09-30")) |>
  dplyr::arrange(data) |> 
  e_charts(data) |>
  e_area(
    volume,
    name = "Volume (%)",
    smooth = T,
    barWidth = "70%",  # ✅ barras mais largas e bem visíveis
    itemStyle = list(
      color = "#1f77b4",
      borderRadius = c(4, 4, 0, 0)
    )
  ) |>
  e_y_axis(
    min = 0,           
    max = 100,
    axisLine = list(show = FALSE),
    axisLabel = list(color = "#333"),
    splitLine = list(lineStyle = list(color = "rgba(0,0,0,0.1)"))
  ) |>
  e_x_axis(
    axisLine = list(show = TRUE, lineStyle = list(color = "#666", width = 1.5)),
    axisLabel = list(color = "#333", fontWeight = "bold"),
    splitLine = list(show = FALSE)
  ) |>
  e_title(
    text = paste0(unique(df$Sistema[df$Sistema == "Cantareira"]), "\n", "Volume Útil Retirado (%)"),  # ✅ título automático em 2 linhas
    textStyle = list(
      fontSize = 18,
      fontWeight = "bold"
    ),
    left = "center",
    top = "5%"
  ) |>
  e_tooltip(
    trigger = "axis"
  ) |>
  e_legend(show = FALSE) |>
  e_grid(left = "8%", right = "5%", bottom = "10%", top = "20%") |>
  e_locale("pt-BR")

# vazao retirada ----
df |>
  filter(Sistema == "Cantareira") |>
  filter(data >= as.Date("2025-01-01") & data <= as.Date("2025-09-30")) |>
  dplyr::arrange(data) |> 
  e_charts(data) |>
  e_area(
    vazao_captada,
    smooth = T,
    name = "m3/s",
    barWidth = "70%",  
    itemStyle = list(
      color = "#1f77b4",
      borderRadius = c(4, 4, 0, 0)
    )
  ) |>
  e_y_axis(
    min = 0,  
    axisLine = list(show = FALSE),
    axisLabel = list(color = "#333"),
    splitLine = list(lineStyle = list(color = "rgba(0,0,0,0.1)"))
  ) |>
  e_x_axis(
    axisLine = list(show = TRUE, lineStyle = list(color = "#666", width = 1.5)),
    axisLabel = list(color = "#333", fontWeight = "bold"),
    splitLine = list(show = FALSE)
  ) |>
  e_title(
    text = paste0(unique(df$Sistema[df$Sistema == "Cantareira"]), "\n", "Vazão captada [m3/s]"),  # ✅ título automático em 2 linhas
    textStyle = list(
      fontSize = 18,
      fontWeight = "bold"
    ),
    left = "center",
    top = "5%"
  ) |>
  e_tooltip(
    trigger = "axis"
  ) |>
  e_legend(show = FALSE) |>
  e_grid(left = "8%", right = "5%", bottom = "10%", top = "20%") |>
  e_locale("pt-BR")

# chuva ----
df |>
  filter(Sistema == "Cantareira") |>
  filter(data >= as.Date("2025-01-01") & data <= as.Date("2025-09-30")) |>
  dplyr::arrange(data) |> 
  e_charts(data) |>
  e_area(
    pluviometria_mensal,
    smooth = T,
    name = "mm",
    barWidth = "70%",  
    itemStyle = list(
      color = "#1f77b4",
      borderRadius = c(4, 4, 0, 0)
    )
  ) |>
  e_line(
    pluviometria_mensal_media,
    smooth = T,
    name = "Média histórica [mm]",
    symbol = "circle",
    symbolSize = 8,
    lineStyle = list(width = 3, color = "#ff7f0e"),
    itemStyle = list(color = "#ff7f0e")
  ) |> 
  e_y_axis(
    min = 0,  
    axisLine = list(show = FALSE),
    axisLabel = list(color = "#333"),
    splitLine = list(lineStyle = list(color = "rgba(0,0,0,0.1)"))
  ) |>
  e_x_axis(
    axisLine = list(show = TRUE, lineStyle = list(color = "#666", width = 1.5)),
    axisLabel = list(color = "#333", fontWeight = "bold"),
    splitLine = list(show = FALSE)
  ) |>
  e_title(
    text = paste0(unique(df$Sistema[df$Sistema == "Cantareira"]), "\n", "Chuva acumulada no mês [mm] e média hitórica mensal [mm]"),  
    textStyle = list(
      fontSize = 18,
      fontWeight = "bold"
    ),
    left = "center",
    top = "5%"
  ) |>
  e_tooltip(
    trigger = "axis",
  ) |>
  e_legend(show = FALSE) |>
  e_grid(left = "8%", right = "5%", bottom = "10%", top = "20%") |>
  e_locale("pt-BR")

# vazao natural -----
df |>
  filter(Sistema == "Cantareira") |>
  filter(data >= as.Date("2025-01-01") & data <= as.Date("2025-09-30")) |>
  dplyr::arrange(data) |> 
  e_charts(data) |>
  e_area(
    vazao_natural_mensal,
    smooth = T,
    name = "m3/s",
    barWidth = "70%",  
    itemStyle = list(
      color = "#1f77b4",
      borderRadius = c(4, 4, 0, 0)
    )
  ) |>
  e_line(
    vazao_natural_media,
    smooth = T,
    name = "Média histórica [m3/s]",
    symbol = "circle",
    symbolSize = 8,
    lineStyle = list(width = 3, color = "#ff7f0e"),
    itemStyle = list(color = "#ff7f0e")
  ) |> 
  e_y_axis(
    min = 0,  
    axisLine = list(show = FALSE),
    axisLabel = list(color = "#333"),
    splitLine = list(lineStyle = list(color = "rgba(0,0,0,0.1)"))
  ) |>
  e_x_axis(
    axisLine = list(show = TRUE, lineStyle = list(color = "#666", width = 1.5)),
    axisLabel = list(color = "#333", fontWeight = "bold"),
    splitLine = list(show = FALSE)
  ) |>
  e_title(
    text = paste0(unique(df$Sistema[df$Sistema == "Cantareira"]), "\n", "Vazão Natural no Mês (m³/s) e Vazão Natural Média Histórica (m³/s)"),  
    textStyle = list(
      fontSize = 18,
      fontWeight = "bold"
    ),
    left = "center",
    top = "5%"
  ) |>
  e_tooltip(
    trigger = "axis",
  ) |>
  e_legend(show = FALSE) |>
  e_grid(left = "8%", right = "5%", bottom = "10%", top = "20%") |>
  e_locale("pt-BR")

