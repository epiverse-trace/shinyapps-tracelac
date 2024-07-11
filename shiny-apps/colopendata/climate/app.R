library(shiny)
library(ColOpenData)
library(shinycssloaders)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Climate"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, 
               textInput("code", "Introduzca código DIVIPOLA")
        )
      ),
      fluidRow(
        column(6, 
               dateInput("start_date", "Fecha de inicio (YYYY-MM-DD)", value = NULL, min = NULL, max = NULL,
                         format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                         language = "en")),
        column(6, 
               dateInput("end_date", "Fecha de fin (YYYY-MM-DD)", value = NULL, min = NULL, max = NULL,
                         format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                         language = "en")
        )
      ),

      fluidRow(
        column(12, 
               selectInput("dropdown_variable", "Seleccione variable:", 
                           choices = list("Temperatura seca (ambiente)" = "TSSM_CON", 
                                          "Temperatura húmeda" = "THSM_CON", 
                                          "Temperatura mínima" = "TMN_CON", 
                                          "Temperatura máxima" = "TMX_CON", 
                                          "Temperatura seca (ambiente) termógrafo" = "TSTG_CON", 
                                          "Humedad Relativa" = "HR_CAL", 
                                          "Humedad Relativa hidrógrafo" = "HRHG_CON", 
                                          "Tension de vapor" = "TV_CAL", 
                                          "Punto de rocío" = "TPR_CAL", 
                                          "Precipitación diaria" = "PTPM_CON", 
                                          "Precipitación horaria" = "PTPG_CON", 
                                          "Evaporación" = "EVTE_CON", 
                                          "Fenómeno Atmosférico" = "FA_CON", 
                                          "Nubosidad" = "NB_CON", 
                                          "Trayectoria del viento" = "RCAM_CON",
                                          "Recorrido solar" = "BSHG_CON", 
                                          "Velocidad del viento" = "VVAG_CON", 
                                          "Dirección del viento" = "DVAG_CON", 
                                          "Velocidad máxima del viento" = "VVMXAG_CON", 
                                          "Dirección máxima del viento" = "DVMXAG_CON"))
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
      shinyjs::useShinyjs(),
      (h3("Previsualización de la información")),
      tableOutput("tssm_table"),
      conditionalPanel(
        condition = "input.button_preview > 0",
        div(id = "spinnerDiv", withSpinner(plotOutput("plot"), color="red")),
      ),
      
      textOutput("error_message")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive values to store results
  result <- reactiveVal(NULL)
  error_message <- reactiveVal(NULL)
  
  
  fetchData <- function() {
    req(input$code, input$start_date, input$end_date, input$dropdown_variable, input$dropdown_format)
    tryCatch({
      downloaded_data <- download_climate(input$code, input$start_date, input$end_date, input$dropdown_variable)
      if (is.null(downloaded_data) || nrow(downloaded_data) == 0) {
        stop("No data available for the specified parameters.")
      }
      result(downloaded_data)
      error_message(NULL)  # Clear previous error messages
    }, error = function(e) {
      result(NULL)  # Clear previous results
      error_message(as.character(e$message))
    }, finally = {
      hide("spinnerDiv")
      
    })
  }
  
  # Function to download data and update reactive value
  observeEvent(input$button_preview, {
    fetchData()
  })
  
  #Render table
  output$tssm_table <- renderTable({
    req(result())
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
          write.xlsx(data, file)
        } else if (input$dropdown_format == "json") {
          write_json(data, file)
        }
      }
    }
  )
  
  output$error_message <- renderText({
    error_message()
  })

}

# Run the application
app <- shinyApp(ui = ui, server = server)
#runApp(app, host ="0.0.0.0", port = 8180, launch.browser = TRUE)
