library(shiny)
library(ColOpenData)
library(shinycssloaders)
library(shinyjs)

ui <- fluidPage(
  titlePanel("Geoespaciales"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      width = 4,
      selectInput("dropdown", "Modulo que desea consultar", 
                  choices = list("Departamento" = "department", 
                                 "Municipio" = "municipality",
                                 "Municipio con clase" = "municipality_class",
                                 "Sector urbano" = "urban_sector",
                                 "Sector rural" = "rural_sector",
                                 "Sección urbana" = "urban_section",
                                 "Sección rural" = "rural_section",
                                 "Zona urbana" = "urban_zone",
                                 "Bloque" = "block")),
      
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
      verbatimTextOutput("dataset"),
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
  
  fetchData <- function() {
    req(input$dropdown, input$dropdown_format)
    show("spinnerDiv")
    downloaded_data <- download_geospatial(input$dropdown, TRUE, FALSE)
    result(downloaded_data)
    hide("spinnerDiv")
  }
  
  # Function to download data and update reactive value
  observeEvent(input$button_preview, {
    fetchData()
  })
  
  # Render Raw Data
  output$dataset <- renderPrint({
    result()
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
      
      if (is.null(data) || length(data) == 0) {
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
