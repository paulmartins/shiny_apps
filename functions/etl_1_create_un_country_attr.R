#' @description Creates a table un_country_attr that contains the columns:
#' country | code | development_index | income_index | region | sub_region
#' The table is saved as RDS such that the columns are kept as factors upon calling readRDS

create_un_country_attr <- function(){
  
  
  # 1 Column names and rows selection -------------------------------------------------------------
  flog.info('Read raw data')
  
  un_country_attr <- read_xlsx(  path='../data_raw/UN_MigrantStockByOriginAndDestination_2019.xlsx'
                               , sheet='ANNEX'
                               , range=c('B16:O299')
                               )
  un_country_attr <- setDT(as.data.frame(un_country_attr))
  setnames(  un_country_attr
           , old=c('Region, subregion, country or area', 'Code')
           , new=c('country', 'code')
           )
  un_country_attr <- un_country_attr[22:nrow(un_country_attr)]


  # 2 Fillling region and subregion -----------------------------------------------------------
  flog.info('Fillling region and subregion')
  
  un_country_attr[, `:=`(region='', sub_region='')]
  for(i in seq(nrow(un_country_attr))){
    # check region names, ie continent
    if(toupper(un_country_attr[i, country]) == un_country_attr[i, country]) {
      region <- un_country_attr[i, country]
      sub_region <- stringr::str_to_title(un_country_attr[i, country], locale = "en")
    }
    # check subregion names
    if(un_country_attr[i, code] > 900 & toupper(un_country_attr[i, country]) != un_country_attr[i, country]) {
      sub_region <- un_country_attr[i, country]
    }
    # fill columns
    flog.trace(paste('Row:', i , 'Region:', region, 'Subregion:', sub_region))
    un_country_attr[i]$region <- region
    un_country_attr[i]$sub_region <- sub_region
  }
  
  
  # 3 Converting country names and boolean cols ---------------------------------------------------
  flog.info('Converting country names and boolean cols ')
   
  un_country_attr[, country:=countrycode(code, origin='un', destination='country.name')]
  bool_cols <- c('More Developed Regions', 'Less Developed Regions', 'Least developed countries',
                 'High-income Countries', 'Middle-income Countries', 
                 'Upper-middle-income Countries', 'Lower-middle-income Countries',
                 'Low-income Countries', 'Sub-Saharan Africa')
  YN_to_boolean <- function(vec_x){
    sapply(vec_x, function(x){
      switch(x, "Y" = TRUE, "N" = FALSE, FALSE)
    })
  }
  un_country_attr[, (bool_cols):=lapply(.SD, YN_to_boolean), .SDcols=bool_cols]
  
  
  # 4 Removing unused rows and cols ---------------------------------------------------------------
  flog.info('Removing unused rows and cols')
    
  un_country_attr <- un_country_attr[code < 900]
  un_country_attr[, Notes:=NULL]
    
    
  # 5 Create development index --------------------------------------------------------------------
  flog.info('Create development index')
  
  un_country_attr[(`More Developed Regions`), development_index:='More developed']
  un_country_attr[(`Less Developed Regions`), development_index:='Less developed']
  un_country_attr[(`Least developed countries`), development_index:='Least developed']
  if(nrow(un_country_attr[is.na(development_index)])>0) {
    flog.warn(paste('Found', nrow(un_country_attr[is.na(development_index)]), 'countries without develop_index'))
  }
  un_country_attr[is.na(development_index), development_index:='No info']
  un_country_attr[, c('More Developed Regions', 'Less Developed Regions', 'Least developed countries'):=NULL]
    
    
  # 6 Create income index -------------------------------------------------------------------------
  flog.info('Create income index')
  
  un_country_attr[(`High-income Countries`), income_index:='High-income']
  un_country_attr[(`Upper-middle-income Countries`), income_index:='Upper-middle-income']
  un_country_attr[(`Lower-middle-income Countries`), income_index:='Lower-middle-income']
  un_country_attr[(`Low-income Countries`), income_index:='Low-income']
  if(nrow(un_country_attr[is.na(income_index)])>0){
    flog.warn(paste('Found',nrow(un_country_attr[is.na(income_index)]), 'countries without income_index'))
  }
  un_country_attr[is.na(income_index), income_index:='No info']
  un_country_attr[, c( 'High-income Countries', 'Low-income Countries', 'Middle-income Countries'
                      ,'Upper-middle-income Countries', 'Lower-middle-income Countries', 'Sub-Saharan Africa'):=NULL]

  
  # 7 Converting to factors -----------------------------------------------------------------------
  flog.info('Converting to factors')
  
  un_country_attr[, development_index:=factor(development_index, levels=c(  'More developed'
                                                                          , 'Less developed'
                                                                          , 'Least developed'
                                                                          , 'No info'
                                                                          )
                                              )
                  ]
  un_country_attr[, income_index:=factor(income_index, levels=c(  'High-income'
                                                                , 'Upper-middle-income'
                                                                , 'Lower-middle-income'
                                                                , 'Low-income'
                                                                , 'No info'
                                                                )
                                         )
                  ]
  un_country_attr[, region:=factor(region, levels=c(  "EUROPE"
                                                    , "NORTHERN AMERICA"
                                                    , "LATIN AMERICA AND THE CARIBBEAN"
                                                    , "SUB-SAHARAN AFRICA"
                                                    , "NORTHERN AFRICA AND WESTERN ASIA"
                                                    , "CENTRAL AND SOUTHERN ASIA"
                                                    , "EASTERN AND SOUTH-EASTERN ASIA"
                                                    , "OCEANIA"
                                                    )
                                   )
                  ]
  
  
  # 8 Writing formatted data ----------------------------------------------------------------------
  flog.info('Saving un_country_attr in data/un_country_attributes.rds')
  
  if(!dir.exists('../data')) dir.create('../data')
  # need to use RDS as data.table::fwrite will loose the factor columns
  saveRDS(un_country_attr, '../data/un_country_attributes.rds')
}
