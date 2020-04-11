# Install R version 3.6.3
FROM rocker/shiny:latest


# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev


# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "install.packages(c( \
  'devtools', \
  'shiny', \
  'shinydashboard', \
  'plotly', \
  'leaflet', \
  'leaflet.extras', \
  'shinyjs', \
  'shinyWidgets', \
  'networkD3', \
  'htmlwidgets', \
  'scales', \
  'DT', \
  'data.table', \
  'readxl', \
  'stringi', \
  'stringr', \
  'futile.logger', \
  'rgeos', \
  'lwgeom', \
  'sf', \
  'countrycode', \
  'RColorBrewer', \
  'rgdal', \
  'gridBase', \
  'circlize' \
  ), repos='http://cran.rstudio.com/')"
  
RUN R -e "devtools::install_github('MarkEdmondson1234/gentelellaShiny')"
RUN R -e "devtools::install_github('jokergoo/ComplexHeatmap')"
# RUN R -e "devtools::install_github('ramnathv/rCharts')"

# Copy configuration files into the Docker image
COPY /app /srv/shiny-server/
RUN sudo chown -R shiny:shiny /var/lib/shiny-server


CMD ["/usr/bin/shiny-server.sh"]