library(data.table)
library(rgdal)
library(sf)
library(stringi)
library(futile.logger)
library(rgeos)
library(countrycode)


load_countries_polygons <- function(force_download = FALSE, force_format = FALSE){
  
  if(force_download){
    # 1. Downloading countries polygons -----------------------------------------------------------
    flog.info('Downloading Countries polygons')
    
    temp <- tempfile()
    url <- 'https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/ref-countries-2016-01m.shp.zip'
    download.file(url, temp)
    unzip(zipfile = temp, exdir = './data/temp-polygons/')
    unzip(zipfile = './data/temp-polygons/CNTR_RG_01M_2016_4326.shp.zip', exdir = './data/polygons/')
    unlink('./data/temp-polygons', recursive = TRUE)
  }

  if(!file.exists('./data/polygons/countries_formatted.shp') | force_format | force_download){
    # 2. Formatting polygons attributes -----------------------------------------------------------
    flog.info('Formatting polygons attributes')
    
    countries_poly <- readOGR(dsn=path.expand("./data/polygons/"), layer = 'CNTR_RG_01M_2016_4326')
    countries_poly$CNTR_NAME <- countrycode(unique(countries_poly$ISO3_CODE),'iso3c','country.name')
    countries_poly$UN_CODE <- countrycode(unique(countries_poly$ISO3_CODE),'iso3c','un')
    countries_poly <- countries_poly[!is.na(countries_poly$CNTR_NAME),]
    countries_poly$NAME_ENGL <- NULL
    countries_poly$FID <- NULL
    countries_poly$CNTR_ID <- NULL
    writeOGR(countries_poly, './data/polygons', "countries_formatted", driver="ESRI Shapefile", overwrite_layer = force_format)
  }
  
  # 3. Reading formatted polygons -----------------------------------------------------------------
  flog.info('Reading formatted polygons')
  
  return(readOGR(dsn=path.expand("./data/polygons/"), layer = 'countries_formatted'))
}
