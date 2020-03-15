library(data.table)
library(readxl)
library(countrycode)
library(futile.logger)
library(stringr)
library(RColorBrewer)
library(rCharts)

library(rgdal)
library(sf)
library(stringi)
library(rgeos)
library(lwgeom)

library(networkD3)
library(htmlwidgets)
library(scales)
library(DT)

download_raw_data <- function(){
  if(!dir.exists('../data_raw')) dir.create('../data_raw')
  
  # 1 Total international migrant stock -----------------------------------------------------------
  flog.info('Downloading UN_MigrantStockTotal_2019.xlsx') 
  url <- 'https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockTotal_2019.xlsx'
  download.file(url, '../data_raw/UN_MigrantStockTotal_2019.xlsx')
  
  # 2 By age and sex ------------------------------------------------------------------------------
  flog.info('Downloading UN_MigrantStockByAgeAndSex_2019.xlsx') 
  url <- 'https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByAgeAndSex_2019.xlsx'
  download.file(url, '../data_raw/UN_MigrantStockByAgeAndSex_2019.xlsx')
  
  # 3 By destination and origin -------------------------------------------------------------------
  flog.info('Downloading UN_MigrantStockByOriginAndDestination_2019.xlsx') 
  url <- 'https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2019.xlsx'
  download.file(url, '../data_raw/UN_MigrantStockByOriginAndDestination_2019.xlsx')
  
  # 4 Countries polygons -----------------------------------------------------------
  flog.info('Downloading countries polygons')
  #https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/ref-countries-2016-60m.shp.zip
  temp <- tempfile()
  url <- 'https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/ref-countries-2016-60m.shp.zip'
  download.file(url, temp)
  unzip(zipfile = temp, exdir = '../data_raw/temp-polygons/')
  unzip(zipfile = '../data_raw/temp-polygons/CNTR_RG_60M_2016_4326.shp.zip', exdir = '../data_raw/polygons/')
  unlink('../data_raw/temp-polygons', recursive = TRUE)
}
