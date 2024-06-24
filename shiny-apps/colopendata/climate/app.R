library(shiny)
library(climate)
library(ColOpenData)
library(shinycssloaders)

ui <- fluidPage(
  titlePanel("Shiny App Climate"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, 
               textInput("code", "Introduzca código DIVIPOLA")
        )
      ),
      fluidRow(
        column(6, 
               textInput("start_date", "Fecha de inicio (YYYY-MM-DD)")),
        column(6, 
               textInput("end_date", "Fecha fin (YYYY-MM-DD)")
        )
      ),
      fluidRow(
        column(12, 
               selectInput("dropdown_variable", "Seleccione variable:", 
                           choices = list("Temperatura seca (ambiente)" = "opt1", 
                                          "Temperatura húmeda" = "opt2", 
                                          "Temperatura mínima" = "opt3", 
                                          "Temperatura máxima" = "opt4", 
                                          "Temperatura seca (ambiente) termógrafo" = "opt5", 
                                          "Humedad Relativa" = "opt6", 
                                          "Humedad Relativa hidrógrafo" = "opt7", 
                                          "Tension de vapor" = "opt8", 
                                          "Punto de rocío" = "opt9", 
                                          "Precipitación diaria" = "opt10", 
                                          "Precipitación horaria" = "opt11", 
                                          "Evaporación" = "opt12", 
                                          "Fenómeno Atmosférico" = "opt13", 
                                          "Nubosidad" = "opt14", 
                                          "Recorrido solar" = "opt15", 
                                          "Velocidad del viento" = "opt16", 
                                          "Dirección del viento" = "opt17", 
                                          "Velocidad máxima del viento" = "opt18", 
                                          "Dirección máxima del viento" = "opt19"))
        )
      ),
      fluidRow(
        column(12, 
               selectInput("dropdown_format", "Seleccione formato", 
                           choices = list("CSV" = "csv", 
                                          "XLSX" = "xlsx", 
                                          "JSON" = "json"))
        )
      ),
      fluidRow(
        column(6, 
               actionButton("button_preview", "Previsualizar")),
        column(6, 
               downloadButton("button_download", "Descargar"))
      ),
      fluidRow(
        column(12,
               p("Es importante hacer click en Previsualizar antes de poder descargar los datos"),
               style = "margin-top: 20px;"
        )
      )
      
    ),
    
    # Main panel for displaying outputs (empty in this case)
    mainPanel(
      h3("Previsualización de la información"),
      withSpinner(tableOutput("tssm_table"), color = "#FF0000")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive values to store results
  result <- reactiveVal(NULL)
  
  # Function to download data and update reactive value
  observeEvent(input$button_preview, {
    req(input$code, input$start_date, input$end_date, input$dropdown_variable, input$dropdown_format)

    downloaded_data <- download_climate(input$code, input$start_date, input$end_date, "TSSM_CON")
    result(downloaded_data)
  })
  
  #Render table
  output$tssm_table <- renderTable({
    head(result())
  })
  
  # Download handler
  output$button_download <- downloadHandler(
    filename = function() {
      paste("climate_data_", Sys.Date(), ".", input$dropdown_format, sep = "")
    },
    content = function(file) {
      data <- result()
      
      if (is.null(data) || nrow(data) == 0) {
        showModal(modalDialog(
          title = "No data available",
          "There is no data available to download.",
          easyClose = TRUE
        ))
      } else {
        if (input$dropdown_format == "csv") {
          write.csv(data, file, row.names = FALSE)
        } else if (input$dropdown_format == "xlsx") {
          library(openxlsx)
          write.xlsx(data, file)
        } else if (input$dropdown_format == "json") {
          library(jsonlite)
          write_json(data, file)
        }
      }
    }
  )
  

  

}

# Run the application
shinyApp(ui = ui, server = server)
