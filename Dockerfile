FROM rocker/r-ver:3.6.1

RUN apt-get update && apt-get install -y \
    sudo \
    pkg-config \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev \
    libgdal-dev \
    liblwgeom-dev \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev
    
# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown'), repos='$MRAN')" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    chown shiny:shiny /var/lib/shiny-server

# Install R packages that are required
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
  'countrycode', \
  'RColorBrewer', \
  'gridBase', \
  'circlize' \
  ), repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('MarkEdmondson1234/gentelellaShiny')"
RUN R -e "devtools::install_github('jokergoo/ComplexHeatmap')"
RUN R -e "install.packages(c('rgdal', 'rgeos', 'sf'))"

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh

# Comment out to use docker-compose and use the local files on docker
COPY un_migration/ /srv/shiny-server/
RUN chmod 777 /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]

