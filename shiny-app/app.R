library(shiny)
library(epiCo)
library(incidence)

data("epi_data") #Datos de ejemplo

ui <- fluidPage(# Application title
  titlePanel("epiCo"),
  sidebarLayout(
    sidebarPanel(
      helpText("Conteo de casos"),
      uiOutput('dropdown'),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "year",
                  label = "Año:",
                  min = 2005,
                  max = 2023,
                  value = 2019)
    ),
    
    mainPanel(
      h3(textOutput("selected_var")),
      plotOutput("populationPyramid"),
      h3("Tasa de incidencia"),
      plotOutput("incidenceRate"),
      h3("Ocupaciones"),
      plotOutput("occupationPlot"),
      h3("Canal endémico"),
      plotOutput("endemicChannel")
    )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  placeList <- 
    list(
      "Espinal" = 73268,
      "Flandes" = 73275,
      "Ibagué" = 73001,
      "Saldaña" = 73671      
    )
  
  output$populationPyramid <- renderPlot({
    selectedPlace <- input$place
    if(is.null(selectedPlace) ){
      selectedPlace <- 73001
    }
    
    data_for_place <<-  epi_data[epi_data$cod_mun_o == selectedPlace, ]

    pyramid <<- population_pyramid(divipola_code = as.numeric(selectedPlace),
                                              year = input$year,
                                              range = 5,
                                              gender = TRUE,
                                              plot = TRUE,
                                              total = TRUE)
  })
  
  output$incidenceRate <- renderPlot({
    place <- input$place
    data_for_year <<-  data_for_place[lubridate::year(data_for_place$fec_not) == input$year, ]
    
    incidence_rate <- age_risk(age = as.integer(data_for_year$edad),
                                    population_pyramid = pyramid,
                                    gender = data_for_year$sexo,
                                    plot = TRUE)
  })
  
  output$occupationPlot <- renderPlot({
    place <- input$place
    year <- input$year
    
    data("isco88_table")
    occupation_plot(isco_codes = as.integer(data_for_year$ocupacion), gender = data_for_year$sexo)
    
  })
  
  output$endemicChannel <- renderPlot({
    place <- input$place
    
    incidence_ibague <- incidence(
      dates = data_for_place$fec_not,
      interval = "1 week"
    )
    
    # Se toma el historico de casos previo al 2021 para construir el canal endémico
    incidence_historic <- incidence_ibague[
      incidence_ibague$date <= as.Date("2018-12-31"), ]
    
    # Se toman el conteo de casos del 2021 como las observaciones
    observations <- incidence_ibague[
      incidence_ibague$date >= as.Date("2019-01-01") &
        incidence_ibague$date <= as.Date("2019-12-31"), ]$counts[,1]
    
    # Se especifican los años hiper endemicos que deben ser ignorados en la 
    # constucción del canal endémico
    outlier_years <- 2016
    
    # Se construye el canal endémico y se plotea el resultado.
    tolima_endemic_chanel <- endemic_channel(
      incidence_historic = incidence_historic,
      observations = observations,
      outlier_years = outlier_years,
      plot = TRUE
    )
    
  })
  
  
  output$selected_var <- renderText({
    paste("Pirámide poblacional para", 
          names(placeList)[match(input$place,placeList)], 
          "en", 
          input$year)
  })
  
  output$dropdown <- renderUI({
    places <- read.table("places.csv", header = TRUE, sep = ";",
                         stringsAsFactors = FALSE)
    selectInput('place', 'Lugar', choices = placeList, selected = 73001)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)