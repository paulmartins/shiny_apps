library(data.table)
library(readxl)
library(countrycode)
library(futile.logger)


load_migration_data <- function(force_download = FALSE, force_format = FALSE){

  if(!file.exists(file.path(getwd(),'data/UN_MigrantStockByOriginAndDestination_2017.xlsx')) | force_download){
    
    # 1 Downloading raw migration data ------------------------------------------------------------
    flog.info('Downloading raw migration data') 
    
    url <- 'https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2017.xlsx'
    download.file(url, './data')
  }
  
  
  if(!file.exists(file.path(getwd(),'data/un_migration.csv')) | force_format | force_download){
    # 2 Formatting raw migration data -------------------------------------------------------------
    
    un_data <- read_xlsx(path = './data/UN_MigrantStockByOriginAndDestination_2017.xlsx'
                         ,sheet = 'Table 1'
                         ,range = c('A16:IG1906'))
    un_data <- setDT(as.data.frame(un_data))
    setnames(un_data
             ,old=c('...1', '...2', '...3', '...4', '...5', '...6')
             ,new=c('year', 'sort_order', 'country_name', 'notes', 'country_to_code', 'data_type'))
    
    # 2.1 Formatting names for country_to ---------------------------------------------------------
    flog.info('Formatting names for country_to')
    
    un_data <- un_data[, c('notes', 'data_type', 'sort_order'):=NULL]
    un_data <- un_data[country_to_code < 900]
    un_data[,country_to := countrycode(country_to_code, origin = 'un', destination = 'country.name')]
    code_mapping <- unique(un_data[,.(country_to_code, country_to, country_name)])
    flog.info(paste('Removing unmatched countries:', un_data[is.na(country_to), unique(country_name)]))
    un_data <- un_data[!is.na(country_to)]
    un_data[, country_name:=NULL]


    # 2.2 Melt into 1 row per observation ---------------------------------------------------------
    flog.info('Melt into 1 row per observation')

    un_data <- melt(un_data
                    ,id.vars = c('year', 'country_to', 'country_to_code', 'Total')
                    ,variable.name = 'country_name_from')


    # 2.3 Formatting names for country_from -------------------------------------------------------
    flog.info('Formatting names for country_from')

    # data.table left join
    setkey(code_mapping, country_name)
    setkey(un_data, country_name_from)
    un_data <- code_mapping[,.(country_name, country_from_code=country_to_code, country_from=country_to)][un_data]
    flog.info(paste('Removing unmatched countries:', un_data[is.na(country_from), unique(country_name)]))
    un_data <- un_data[!is.na(country_from)]
    un_data[, country_name:=NULL]


    # 2.4 Replacing no values with 0 -------------------------------------------------------------
    flog.info('Replacing no values with 0')

    un_data[Total == '..', Total:=0]
    un_data[, Total:=as.numeric(Total)]
    un_data[value == '..', value:=0]
    un_data[, value:=as.numeric(value)]

    setcolorder(un_data, c('year','country_from', 'country_from_code', 'country_to', 'country_to_code', 'value', 'Total'))


    # 2.5 Writing formatted data ------------------------------------------------------------------
    flog.info('Writing formatted data')

    fwrite(un_data, './data/un_migration.csv')
  }
  
  # 3 Read formatted data -----------------------------------------------------------------------
  flog.info('Read formatted data')
  
  return(fread('./data/un_migration.csv'))
}

