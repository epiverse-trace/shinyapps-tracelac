library(shiny)
library(ColOpenData)
library(shinyjs)
library(shinycssloaders)
ui <- fluidPage(
  
  titlePanel("Shiny App Population projections"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dropdown_level", "Select an option:",
                  choices = list("Nacional" = "national", 
                                 "Departamento" = "department", 
                                 "Municipio" = "municipality")),
      
      fluidRow(
        column(6,
               numericInput("year1", "Año inicio", value = 2020)),
        column(6,
               numericInput("year2", "Año fin", value = 2030))
      ),
      
      checkboxInput("checkbox_sex", "Incluir división por sexo", value = FALSE),
      
      conditionalPanel(
        condition = "input.dropdown_level == 'municipality'",
        checkboxInput("checkbox_etnia", "Incluir división por etnia (solo para municipio)", value = FALSE)
      ),
      
      
      selectInput("dropdown_format", "Seleccione formato", 
                  choices = list("CSV" = "csv", 
                                 "XLSX" = "xlsx", 
                                 "JSON" = "json")),
      
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
    
    mainPanel(
      h3("Previsualización de la información"),
      tableOutput("pop_proj_head") %>% withSpinner(color = "#FF0000")
    )
  )
)


# Define the server logic
server <- function(input, output, session) {
  
  # Reactive values to store results
  result <- reactiveVal(NULL)
  
  
  # Function to download data and update reactive value
  observeEvent(input$button_preview, {
    req(input$dropdown_level, input$year1, input$year2, input$dropdown_format)
    
    # Update checkbox_etnia based on dropdown_level
    if (input$dropdown_level == "municipality") {
      updateCheckboxInput(session, "checkbox_etnia", value = FALSE)
      downloaded_data <- download_pop_projections(input$dropdown_level, input$year1, input$year2, input$checkbox_sex, input$checkbox_etnia)
    } else {
      downloaded_data <- download_pop_projections(input$dropdown_level, input$year1, input$year2, input$checkbox_sex)
    }
    
    result(downloaded_data)
  })
  
  # Render Table
  output$pop_proj_head <- renderTable({
    head(result())
  })
  
  
  # Download handler
  output$button_download <- downloadHandler(
    filename = function() {
      paste("population_projection_", Sys.Date(), ".", input$dropdown_format, sep = "")
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
