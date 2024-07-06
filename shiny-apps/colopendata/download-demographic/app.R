library(shiny)
library(ColOpenData)
library(shinycssloaders)
library(shinyjs)

ui <- fluidPage(
  titlePanel("Descargar información demografica"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      width = 4,
      textInput("name", "Introduzca nombre del dataset"),
      
      selectInput("dropdown_format", "Seleccione formato", 
                  choices = list("CSV" = "csv", 
                                 "XLSX" = "xlsx", 
                                 "JSON" = "json")),
      
      fluidRow(
        column(6, 
               actionButton("button_preview", "Previsualizar")),
        column(6, 
               downloadButton("button_download", "Descargar"))
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      shinyjs::useShinyjs(),
      h3("Previsualización de la información"),
      uiOutput("error_message"),
      tableOutput("dataset"),
      conditionalPanel(
        condition = "input.button_preview > 0",
        div(id = "spinnerDiv", withSpinner(plotOutput("plot"), color="red"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive value to store result
  result <- reactiveVal()
  error_message <- reactiveVal("")
  
  fetchData <- function() {
    req(input$name, input$dropdown_format)
    tryCatch({
      downloaded_data <- download_demographic(input$name)
      result(downloaded_data)
      error_message("") # Clear error message if data is fetched successfully
      hide("spinnerDiv")
    }, error = function(e) {
      error_message("Error: `dataset` name format is not correct")
      result(NULL) # Clear any previous data
      hide("spinnerDiv")
    })
  }
  
  # Function to download data and update reactive value
  observeEvent(input$button_preview, {
    fetchData()
  })
  
  # Render Table
  output$dataset <- renderTable({
    head(result())
  })
  
  # Render error message
  output$error_message <- renderUI({
    if (error_message() != "") {
      tags$div(style = "color: red;", error_message())
    }
  })
  
  # Download handler
  output$button_download <- downloadHandler(
    filename = function() {
      paste("dataset", Sys.Date(), ".", input$dropdown_format, sep = "")
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
}

# Run the application
shinyApp(ui = ui, server = server)
