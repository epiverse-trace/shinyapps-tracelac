# Load the shiny package
library(shiny)
library(ColOpenData)

ui <- fluidPage(
  titlePanel("Shiny App Divipola"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      width = 4,
      # Dropdown menu (Tipo de consulta)
      selectInput("dropdown", "Tipo de consulta", 
                  choices = list("Nombre a código de departamento" = "name_to_code_dep", 
                                 "Nombre a código municipio" = "name_to_code_mun", 
                                 "Código a nombre departamento" = "cod_to_name_dep",
                                 "Código a nombre municipio" = "cod_to_name_mun",
                                 "Nombre a nombre departamento" = "name_to_name_dep",
                                 "Nombre a nombre municipio" = "name_to_name_mun")),
      
      # Text input field
      textInput("text", "Introduzca parámetro de consulta"),
      
      conditionalPanel(
        condition = 'input.dropdown == "name_to_code_mun" || input.dropdown == "name_to_name_mun"',
        textInput("text2", "Introduzca segundo parámetro de consulta")
      ),
      
      # Button
      actionButton("button", "Consultar")
    ),
    
    # Main panel for displaying outputs (empty in this case)
    mainPanel(
      h3("Resultados de la consulta"),
      verbatimTextOutput("results")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive value to store result
  result <- reactiveVal("Esperando consulta...")
  
  # Observe button click
  observeEvent(input$button, {
    req(input$text, input$dropdown)  # Ensure text input and dropdown selection are not empty
    
    # Switch based on dropdown selection to call appropriate function
    switch(input$dropdown,
           "name_to_code_dep" = {
             result(name_to_code_dep(input$text))
           },
           "name_to_code_mun" = {
             req(input$text2)
             result(name_to_code_mun(input$text, input$text2))
           },
           "cod_to_name_dep" = {
             result(code_to_name_dep(input$text))
           },
           "cod_to_name_mun" = {
             result(code_to_name_mun(input$text))
           },
           "name_to_name_dep" = {
             result(name_to_standard_dep(input$text))
           },
           "name_to_name_mun" = {
             req(input$text2)
             result(name_to_standard_mun(input$text, input$text2))
           }
    )
  })
  
  # Render output
  output$results <- renderText({
    as.character(result())
  })
}


# Run the application
app <- shinyApp(ui = ui, server = server)
runApp(app, host ="0.0.0.0", port = 8180, launch.browser = TRUE)
