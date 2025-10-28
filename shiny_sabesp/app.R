### Packages
library(shiny)
library(tidyverse)
library(echarts4r)
library(bs4Dash)
library(shinycssloaders)

### Box com spinner de carregamento
loadbox <- \(content, width = 12, maxi = T, ...) {
  bs4Dash::column(
    width = width,
    bs4Dash::box(shinycssloaders::withSpinner(content, color = '#ff851b'),
                 ..., maximizable = maxi, width = 12)
  )
}

### Load: 
df <- read_csv2("data/dados_mananciais_completo.csv") |> 
  filter(Sistema %in% c("SIM", "Cantareira", "Alto Tietê", "Guarapiranga"))
min <- min(df$data)
max <- max(df$data)

ui <- bs4DashPage(
  help = FALSE,
  dark = FALSE,
  
  header = bs4DashNavbar(
    title = 'Mananciais SP',
    leftUi = tagList(
      selectInput("sistema", 
                  label = h3("Escolha o sistema:"), 
                  choices = unique(df$Sistema), 
                  selected = "SIM", 
                  multiple = FALSE, 
                  selectize = TRUE), 
      dateRangeInput("dates", 
                     label = h3("Escolha as datas:"), 
                     min = min, 
                     max = max, 
                     start = min, 
                     end = max)
    )
  ),
  
  sidebar = NULL,
  
  body = bs4DashBody(
    conditionalPanel(
      condition = "input.sistema != 'SIM'",
      loadbox(echarts4rOutput("precipitacao", height = "420px"))
    ),
    loadbox(
      title = 'Vazão Natural no Mês (m³/s) e Vazão Natural Média Histórica (m³/s)',
      echarts4rOutput("vazao_natural")
    ),
    loadbox(
      title = 'a',
      echarts4rOutput("volume_armazenado")
    ),
    loadbox(
      title = 'a',
      echarts4rOutput("volume_retirado")
    )
  )
)


# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Mananciais SP"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#           , 
#           hr(),
#           fluidRow(column(12, textOutput("ultima_atualizacao")))
#         ), 
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#           hr(),
#           uiOutput("cabecalho_sistema"),
#           hr(),
#           conditionalPanel(
#             condition = "input.sistema != 'SIM'",
#             echarts4rOutput("precipitacao", height = "420px")
#           ), 
#           echarts4rOutput("vazao_natural"), 
#           echarts4rOutput("volume_armazenado"), 
#           echarts4rOutput("volume_retirado")
#         )
#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Atualizacao:
  output$ultima_atualizacao <- renderText({
    paste("Última atualização:", max(df$data, na.rm = TRUE))
  })
  
  # Cabeçalho com o nome do sistema
  output$cabecalho_sistema <- renderUI({
    req(input$sistema)
    nome <- if (input$sistema == "SIM") "SIM" else input$sistema
    tags$h3(nome)
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
        pluviometria_mensal        = as.numeric(pluviometria_mensal),
        pluviometria_mensal_media  = as.numeric(pluviometria_mensal_media)
      ) |>
      e_charts(data) |>   # <- ajuste principal (ou use e_charts(x = data))
      e_area(
        pluviometria_mensal,
        smooth = TRUE,
        name = "mm",
        itemStyle = list(color = "#1f77b4", borderRadius = c(4,4,0,0))
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
        axisLine = list(show = TRUE, lineStyle = list(color = "#666", width = 1.5)),
        axisLabel = list(color = "#333", fontWeight = "bold"),
        splitLine = list(show = FALSE)
      ) |>
      e_title(
        text = "\nChuva acumulada no mês [mm] e média histórica mensal [mm]",
        textStyle = list(fontSize = 18, fontWeight = "bold"),
        left = "center", top = "5%"
      ) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE) |>
      e_grid(left = "8%", right = "5%", bottom = "10%", top = "20%") |>
      e_locale("pt-BR")
  })
  
    
  ### Vazao Natural:
  output$vazao_natural <- renderEcharts4r({
    req(input$sistema, input$dates)
    
    d <- dados_filtrados()
    
    req(nrow(d) > 0)
    
    d |>
      e_charts(data) |>
      e_area(
        vazao_natural_mensal,
        smooth = TRUE,
        name = "m3/s",
        barWidth = "70%",
        itemStyle = list(
          color = "#1f77b4",
          borderRadius = c(4, 4, 0, 0)
        )
      ) |>
      e_line(
        vazao_natural_media,
        smooth = TRUE,
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
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE) |>
      e_grid(left = "8%", right = "5%", bottom = "10%", top = "20%") |>
      e_locale("pt-BR")
  })
  
  ### Volume Armazenado: 
  
  output$volume_armazenado <- renderEcharts4r({
    req(input$sistema, input$dates)
    
    d <- dados_filtrados()
    
    req(nrow(d) > 0)
    
    titulo <- paste0("\nVolume Útil Retirado (%)")
    
    d |>
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
        axisLine = list(show = TRUE, lineStyle = list(color = "#666", width = 1.5)),
        axisLabel = list(color = "#333", fontWeight = "bold"),
        splitLine = list(show = FALSE)
      ) |>
      e_title(
        text = titulo,
        textStyle = list(fontSize = 18, fontWeight = "bold"),
        left = "center",
        top  = "5%"
      ) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE) |>
      e_grid(left = "8%", right = "5%", bottom = "10%", top = "20%") |>
      e_locale("pt-BR")
  })
  
  ### Volume Retirado: 
  
  output$volume_retirado <- renderEcharts4r({
    req(input$sistema, input$dates)
    
    d <- dados_filtrados()
    
    req(nrow(d) > 0)
    
    titulo <- paste0(
      "\nVazão Captada [m³/s]"
    )
    
    d |>
      e_charts(data) |>
      e_area(
        vazao_captada,
        smooth = TRUE,
        name = "m³/s",
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
        text = titulo,
        textStyle = list(fontSize = 18, fontWeight = "bold"),
        left = "center",
        top  = "5%"
      ) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE) |>
      e_grid(left = "8%", right = "5%", bottom = "10%", top = "20%") |>
      e_locale("pt-BR")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
