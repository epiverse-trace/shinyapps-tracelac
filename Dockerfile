# Base R Shiny image
FROM rocker/shiny

# Make a directory in the container
RUN mkdir /home/shiny-app
RUN apt-get update && apt-get install libudunits2-dev r-cran-devtools -y

# Install R dependencies
RUN R -e "install.packages(c('dplyr', 'ggplot2', 'gapminder','shinyjs','shinycssloaders'))"
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('epiverse-trace/epiCo')"

# Copy the Shiny app code
COPY shiny-apps/epico/app.R /home/shiny-app/epico/app.R

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
#CMD ["R", "-e", "shiny::runApp('/home/shiny-app/epico/app.R')"]
CMD ["R", "-e", "library(shiny); source('/home/shiny-app/epico/app.R')"]