#' @description Creates a table un_migration_flow that contains the columns:
#' year | country_from | country_from_code | country_to | country_to_code | value


create_un_migration_flow <- function(){
  
  
  # 1 Column names and rows selection -------------------------------------------------------------
  flog.info('Read raw data')
  
  un_migration_flow <- read_xlsx(  path = '../data_raw/UN_MigrantStockByOriginAndDestination_2019.xlsx'
                                 , sheet = 'Table 1'
                                 , range = c('A16:IG1997'))
  un_migration_flow <- setDT(as.data.frame(un_migration_flow))
  setnames(un_migration_flow
           ,old=c('...1', '...2', '...3', '...4', '...5', '...6')
           ,new=c('year', 'sort_order', 'country_name', 'notes', 'country_to_code', 'data_type'))
  
  # 2 Formatting names for country_to -------------------------------------------------------------
  flog.info('Formatting names for country_to')
  
  un_migration_flow <- un_migration_flow[, c('notes', 'data_type', 'sort_order'):=NULL]
  un_migration_flow <- un_migration_flow[country_to_code < 900]
  un_migration_flow[, country_to:=countrycode(country_to_code, origin = 'un', destination = 'country.name')]
  code_mapping <- unique(un_migration_flow[,.(country_to_code, country_to, country_name)])
  flog.info(paste('Removing unmatched countries:', un_migration_flow[is.na(country_to), unique(country_name)]))
  un_migration_flow <- un_migration_flow[!is.na(country_to)]
  un_migration_flow[, country_name:=NULL]


  # 3 Melt into 1 row per observation -------------------------------------------------------------
  flog.info('Melt into 1 row per observation')

  un_migration_flow <- melt(  un_migration_flow
                            , id.vars = c('year', 'country_to', 'country_to_code', 'Total')
                            , variable.name = 'country_name_from')


  # 4 Formatting names for country_from -----------------------------------------------------------
  flog.info('Formatting names for country_from')

  # data.table left join
  setkey(code_mapping, country_name)
  setkey(un_migration_flow, country_name_from)
  un_migration_flow <- code_mapping[,.(country_name, country_from_code=country_to_code, country_from=country_to)][un_migration_flow]
  flog.info(paste('Removing unmatched countries:', un_migration_flow[is.na(country_from), unique(country_name)]))
  un_migration_flow <- un_migration_flow[!is.na(country_from)]
  un_migration_flow[, country_name:=NULL]


  # 5 Replacing no values with 0 ------------------------------------------------------------------
  flog.info('Replacing no values with 0')

  un_migration_flow[Total == '..', Total:=0]
  un_migration_flow[, Total:=as.numeric(Total)]
  un_migration_flow[value == '..', value:=0]
  un_migration_flow[, value:=as.numeric(value)]

  setcolorder(un_migration_flow, c('year', 'country_from', 'country_from_code', 'country_to', 'country_to_code', 'value', 'Total'))


  # 6 Writing formatted data ----------------------------------------------------------------------
  flog.info('Saving un_country_attr in data/un_migration_flow.csv')

  fwrite(un_data, '../data/un_migration_flow.csv')
}

