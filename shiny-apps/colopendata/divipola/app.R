# Load the shiny package
library(shiny)
library(ColOpenData)

ui <- fluidPage(
  titlePanel("Divipola"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      width = 4,
      # Dropdown menu (Tipo de consulta)
      
      textOutput("text"),
      selectInput("dropdown", "Tipo de consulta", 
                  choices = list("Nombre a código de departamento" = "name_to_code_dep", 
                                 "Nombre a código municipio" = "name_to_code_mun", 
                                 "Código a nombre departamento" = "cod_to_name_dep",
                                 "Código a nombre municipio" = "cod_to_name_mun",
                                 "Nombre a nombre departamento" = "name_to_name_dep",
                                 "Nombre a nombre municipio" = "name_to_name_mun")),
      
      # Text input field
      textInput("text", "Introduzca el nombre del departamento"),
      
      conditionalPanel(
        condition = 'input.dropdown == "name_to_code_mun" || input.dropdown == "name_to_name_mun"',
        textInput("text2", "Introduzca los nombres de los municipios")
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
server <- function(input, output, session) {
  
  # Reactive value to store result
  result <- reactiveVal("Esperando consulta...")
  
  # Observe dropdown selection and reset text inputs
  observeEvent(input$dropdown, {
    updateTextInput(session, "text", value = "")
    updateTextInput(session, "text2", value = "")
    
    # Update text input label based on dropdown selection
    new_label <- switch(input$dropdown,
                        "name_to_code_dep" = "Introduzca el nombre del departamento o varios nombres de departamentos separados por comas",
                        "name_to_code_mun" = "Introduzca el nombre del departamento o varios nombres de departamentos separados por comas",
                        "cod_to_name_dep" = "Introduzca el código del departamento o varios códigos de departamentos separados por comas",
                        "cod_to_name_mun" = "Introduzca el código del municipio o varios códigos de municipios separados por comas",
                        "name_to_name_dep" = "Introduzca el nombre del departamento o varios nombres de departamentos separados por comas",
                        "name_to_name_mun" = "Introduzca el nombre del departamento o varios nombres de departamentos separados por comas",
                        "Introduzca el nombre del departamento")
    second_label <- switch(input$dropdown,
                          "name_to_code_mun" = "Introduzca el nombre del municipio o varios nombres de municipios separados por comas",
                          "name_to_name_mun" = "Introduzca el nombre del municipio o varios nombres de municipios separados por comas",
                          "Introduzca los nombres de los municipios")
    
    updateTextInput(session, "text", label = new_label)
    updateTextInput(session, "text2", label = second_label)
  })
  
  # Observe button click
  observeEvent(input$button, {
    req(input$text, input$dropdown)  # Ensure text input and dropdown selection are not empty
    
    # Try to execute the appropriate function and handle errors
    result({
      tryCatch({
        switch(input$dropdown,
               "name_to_code_dep" = {
                 elements <- strsplit(input$text, ",\\s*")[[1]]
                 name_to_code_dep(elements)
               },
               "name_to_code_mun" = {
                 req(input$text2)
                 elements <- strsplit(input$text, ",\\s*")[[1]]
                 elements2 <- strsplit(input$text2, ",\\s*")[[1]]
                 name_to_code_mun(elements, elements2)
               },
               "cod_to_name_dep" = {
                 elements <- strsplit(input$text, ",\\s*")[[1]]
                 if (any(nchar(elements) != 2)) stop("Los códigos de los departamentos deben tener exactamente 2 caracteres.")
                 code_to_name_dep(elements)
               },
               "cod_to_name_mun" = {
                 elements <- strsplit(input$text, ",\\s*")[[1]]
                 if (any(nchar(elements) != 5)) stop("Los códigos de los municipios deben tener exactamente 5 caracteres.")
                 code_to_name_mun(elements)
               },
               "name_to_name_dep" = {
                 elements <- strsplit(input$text, ",\\s*")[[1]]
                 name_to_standard_dep(elements)
               },
               "name_to_name_mun" = {
                 req(input$text2)
                 elements <- strsplit(input$text, ",\\s*")[[1]]
                 elements2 <- strsplit(input$text2, ",\\s*")[[1]]
                 name_to_standard_mun(elements, elements2)
               }
        )
      }, error = function(e) {
        # Return the error message
        as.character(e$message)
      })
    })
  })
  
  # Render output
  # output$results <- renderText({
  #   as.character(result())
  # })
  # Render output with improved handling for multiple outputs and 'NA' values
  output$results <- renderText({
    res <- result()
    # Ensure res is treated as a character vector
    res_elements <- unlist(strsplit(as.character(res), " "))
    # Use lapply for potentially more than one element and to handle NA properly
    formatted_elements <- lapply(res_elements, function(element) {
      if (is.na(element)) {
        "El nombre/código no es valido"
      } else {
        element
      }
    })
    # Combine the formatted elements back into a single string
    paste(formatted_elements, collapse = ", ")
  })
  
  
}


# Run the application
app <- shinyApp(ui = ui, server = server)
#runApp(app, host ="0.0.0.0", port = 8180, launch.browser = TRUE)
