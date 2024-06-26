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
      
    ),
    
    # Main panel for displaying outputs (empty in this case)
    mainPanel(
      h3("Previsualización de la información"),
      conditionalPanel(
        condition = "output.showLoadingMessage",
        p("Esperando parametros...", )
      ),
      withSpinner(tableOutput("tssm_table"), color = "#FF0000")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive values to store results
  plotLoaded <- reactiveVal(FALSE)
  buttonClicked <- reactiveVal(FALSE)
  result <- reactiveVal(NULL)
  
  fetchData <- function() {
    req(input$code, input$start_date, input$end_date, input$dropdown_variable, input$dropdown_format)
    plotLoaded(FALSE)
    buttonClicked(TRUE)
    downloaded_data <- download_climate(input$code, input$start_date, input$end_date, "TSSM_CON")
    result(downloaded_data)
    plotLoaded(TRUE)
  }
  
  # Function to download data and update reactive value
  observeEvent(input$button_preview, {
    buttonClicked(TRUE)
    fetchData()
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
      # Check if data is already loaded, if not, fetch data
      if(is.null(result())) {
        fetchData()
        # Wait for data to be fetched before proceeding
        while(is.null(result())) {
          Sys.sleep(0.1)
        }
      }
      
      data <- result()
      
      if (nrow(data) == 0) {
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
  
  output$showLoadingMessage <- reactive({
    req(input$code, input$start_date, input$end_date, input$dropdown_variable, input$dropdown_format)
    buttonClicked() && !plotLoaded()
  })
  
  outputOptions(output, "showLoadingMessage", suspendWhenHidden = FALSE)
  

}

# Run the application
app <- shinyApp(ui = ui, server = server)
runApp(app, host ="0.0.0.0", port = 8180, launch.browser = TRUE)