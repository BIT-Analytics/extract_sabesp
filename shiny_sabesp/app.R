### Packages
library(shiny)
library(tidyverse)
library(echarts4r)
library(bs4Dash)
library(shinycssloaders)
library(shinyWidgets)

### Box com spinner de carregamento
loadbox <- \(content, width = 12, maxi = T, ...) {
  bs4Dash::column(
    width = width,
    bs4Dash::box(
      shinycssloaders::withSpinner(content, color = '#ff851b'),
      ...,
      maximizable = maxi,
      width = 12
    )
  )
}

### Load:
df <- read_csv2("https://raw.githubusercontent.com/BIT-Analytics/extract_sabesp/refs/heads/main/dados_mananciais_completo.csv") |>
  filter(Sistema %in% c("SIM", "Cantareira", "Alto Tietê", "Guarapiranga")) |>
  arrange(Sistema, data)
upd <- df |> 
  summarise(data = max(data), .by = Sistema)
df <- df |>
  mutate(data = floor_date(data, 'month')) |>
  slice(n(), .by = c(Sistema, data))
min <- min(as.Date(df$data))
max <- max(as.Date(df$data))

ui <- bs4DashPage(
  help = FALSE,
  dark = FALSE,

  header = bs4DashNavbar(
    title = h4('Mananciais', br(), 'RMSP'),
    fixed = TRUE,
    pickerInput(
      "sistema",
      label = "SISTEMA",
      choices = unique(df$Sistema),
      selected = "SIM",
      multiple = FALSE
    ),
    airMonthpickerInput(
      inputId = "dates",
      label = "PERÍODO",
      value = c(min, max),
      dateFormat = "MMMM yyyy",
      minDate = min,
      maxDate = max,
      range = TRUE
    )
  ),

  sidebar = bs4DashSidebar(disable = T),

  body = bs4DashBody(
    includeCSS('www/styles.css'),
    loadbox(
      uiOutput('cabecalho_sistema'),
      headerBorder = F,
      maxi = F,
      collapsible = F
    ),
    loadbox(
      title = 'Volume útil retirado (%)',
      echarts4rOutput("volume_armazenado")
    ),
    loadbox(
      title = 'Vazão captada [m³/s]',
      echarts4rOutput("volume_retirado")
    ),
    conditionalPanel(
      condition = "input.sistema != 'SIM'",
      loadbox(
        title = 'Chuva acumulada no mês [mm] e média histórica mensal [mm]',
        echarts4rOutput("precipitacao")
      )
    ),
    loadbox(
      title = 'Vazão Natural no mês (m³/s) e média histórica mensal (m³/s)',
      echarts4rOutput("vazao_natural")
    )
  )
)

# SERVER ----
server <- function(input, output) {
  # Cabeçalho com o nome do sistema
  output$cabecalho_sistema <- renderUI({
    req(input$sistema)
    nome <- if (input$sistema == "SIM") {
      "Integrado Metropolitano"
    } else {
      input$sistema
    }
    tempo <- upd |> 
      dplyr::filter(Sistema == input$sistema) |> 
      dplyr::pull(data)

    tagList(
      div(h3(nome), strong('SISTEMA')),
      div(h5(tempo), strong('ÚLTIMA ATUALIZAÇÃO'))
    )
  })

  # Dados reativos conforme seleção
  dados_filtrados <- reactive({
    req(input$sistema, input$dates)
    df |>
      dplyr::filter(
        Sistema == input$sistema,
        data >= as.Date(input$dates[1]),
        data <= as.Date(input$dates[2])
      ) |>
      dplyr::arrange(data)
  })

  # Gráfico precipitacao:
  output$precipitacao <- renderEcharts4r({
    d <- dados_filtrados()
    req(nrow(d) > 0)

    d |>
      dplyr::mutate(
        pluviometria_mensal = as.numeric(pluviometria_mensal) |>
          round(2),
        pluviometria_mensal_media = as.numeric(pluviometria_mensal_media) |>
          round(2),
        data = format(data, '%b %Y') |> toupper()
      ) |>
      e_charts(data) |>
      e_area(
        pluviometria_mensal,
        smooth = TRUE,
        name = "Chuva acumulada [mm]",
        itemStyle = list(color = "#1f77b4", borderRadius = c(4, 4, 0, 0))
      ) |>
      e_line(
        pluviometria_mensal_media,
        smooth = TRUE,
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
        axisLine = list(
          show = TRUE,
          lineStyle = list(color = "#666", width = 1.5)
        ),
        axisLabel = list(color = "#333", fontWeight = "bold"),
        splitLine = list(show = FALSE)
      ) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE) |>
      e_grid(left = "5%", right = "5%", bottom = "8%", top = "8%") |>
      e_locale("pt-BR")
  })

  ### Vazao Natural:
  output$vazao_natural <- renderEcharts4r({
    req(input$sistema, input$dates)
    d <- dados_filtrados()
    req(nrow(d) > 0)

    d |>
      dplyr::mutate(
        vazao_natural_mensal = round(vazao_natural_mensal, 2),
        data = format(data, '%b %Y') |> toupper()
      ) |>
      e_charts(data) |>
      e_area(
        vazao_natural_mensal,
        smooth = TRUE,
        name = "Vazão natural [m³/s]",
        barWidth = "70%",
        itemStyle = list(
          color = "#1f77b4",
          borderRadius = c(4, 4, 0, 0)
        )
      ) |>
      e_line(
        vazao_natural_media,
        smooth = TRUE,
        name = "Média histórica [m³/s]",
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
        axisLine = list(
          show = TRUE,
          lineStyle = list(color = "#666", width = 1.5)
        ),
        axisLabel = list(color = "#333", fontWeight = "bold"),
        splitLine = list(show = FALSE)
      ) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE) |>
      e_grid(left = "5%", right = "5%", bottom = "8%", top = "8%") |>
      e_locale("pt-BR")
  })

  ### Volume Armazenado:

  output$volume_armazenado <- renderEcharts4r({
    req(input$sistema, input$dates)
    d <- dados_filtrados()
    req(nrow(d) > 0)

    d |>
      dplyr::mutate(
        volume = round(volume, 2),
        data = format(data, '%b %Y') |> toupper()
      ) |>
      e_charts(data) |>
      e_area(
        volume,
        name = "Volume (%)",
        smooth = TRUE,
        barWidth = "70%",
        itemStyle = list(
          color = "#1f77b4",
          borderRadius = c(4, 4, 0, 0)
        )
      ) |>
      e_y_axis(
        min = 0,
        max = 100,
        axisLine = list(show = FALSE),
        axisLabel = list(color = "#333", formatter = "{value}%"),
        splitLine = list(lineStyle = list(color = "rgba(0,0,0,0.1)"))
      ) |>
      e_x_axis(
        axisLine = list(
          show = TRUE,
          lineStyle = list(color = "#666", width = 1.5)
        ),
        axisLabel = list(color = "#333", fontWeight = "bold"),
        splitLine = list(show = FALSE)
      ) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE) |>
      e_grid(left = "5%", right = "5%", bottom = "8%", top = "8%") |>
      e_locale("pt-BR")
  })

  ### Volume Retirado:

  output$volume_retirado <- renderEcharts4r({
    req(input$sistema, input$dates)
    d <- dados_filtrados()
    req(nrow(d) > 0)

    d |>
      dplyr::mutate(
        vazao_captada = round(vazao_captada, 2),
        data = format(data, '%b %Y') |> toupper()
      ) |>
      e_charts(data) |>
      e_area(
        vazao_captada,
        smooth = TRUE,
        name = "Vazão captada [m³/s]",
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
        axisLine = list(
          show = TRUE,
          lineStyle = list(color = "#666", width = 1.5)
        ),
        axisLabel = list(color = "#333", fontWeight = "bold"),
        splitLine = list(show = FALSE)
      ) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE) |>
      e_grid(left = "5%", right = "5%", bottom = "8%", top = "8%") |>
      e_locale("pt-BR")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
