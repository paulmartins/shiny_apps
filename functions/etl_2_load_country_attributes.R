library(data.table)
library(readxl)
library(countrycode)
library(futile.logger)
library(stringr)
library(RColorBrewer)

load_country_attributes <- function(force_download = FALSE, force_format = FALSE){
  
  if(force_download){
    
    # 1 Downloading raw migration data ------------------------------------------------------------
    flog.info('Downloading raw migration data') 
    
    url <- 'https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2019.xlsx'
    download.file(url, '../data')
  }
  
  
  if(force_format | force_download){
    # 2 Formatting raw attributes data -------------------------------------------------------------
    
    un_attr <- read_xlsx(path = '../data/UN_MigrantStockByOriginAndDestination_2019.xlsx'
                         ,sheet = 'ANNEX'
                         ,range = c('B16:O299'))
    un_attr <- setDT(as.data.frame(un_attr))
    setnames(un_attr
             ,old=c('Region, subregion, country or area', 'Code')
             ,new=c('country', 'code'))
    un_attr <- un_attr[22:nrow(un_attr)]

    
    
    # 2.1 Fillling region and subregion -----------------------------------------------------------
    flog.info('Fillling region and subregion')
    
    un_attr[, `:=`(region='', sub_region='')]
    for(i in seq(nrow(un_attr))){
      # check region names, ie continent
      if(toupper(un_attr[i, country]) == un_attr[i, country]) {
        region <- un_attr[i, country]
        sub_region <- stringr::str_to_title(un_attr[i, country], locale = "en")
      }
      # check subregion names
      if(un_attr[i, code] > 900 & toupper(un_attr[i, country]) != un_attr[i, country]) {
        sub_region <- un_attr[i, country]
      }
      # fill columns
      flog.trace(paste('Row:', i , 'Region:', region, 'Subregion:', sub_region))
      un_attr[i]$region <- region
      un_attr[i]$sub_region <- sub_region
    }
    
    
    # 2.2 Converting country names and boolean cols -----------------------------------------------
    flog.info('Converting country names and boolean cols ')
   
    un_attr[,country := countrycode(code, origin = 'un', destination = 'country.name')]
    bool_cols <- c('More Developed Regions', 'Less Developed Regions', 'Least developed countries',
                   'High-income Countries', 'Middle-income Countries', 
                   'Upper-middle-income Countries', 'Lower-middle-income Countries',
                   'Low-income Countries', 'Sub-Saharan Africa')
    YN_to_boolean <- function(vec_x){
      sapply(vec_x, function(x){
        switch(x, "Y" = TRUE, "N" = FALSE, FALSE)
      })
    }
    un_attr[,(bool_cols):=lapply(.SD, YN_to_boolean), .SDcols=bool_cols]
    
    
    # 2.3 Removing unused rows and cols -----------------------------------------------------------
    flog.info('Removing unused rows and cols')
    
    un_attr <- un_attr[code < 900]
    un_attr[,Notes:=NULL]
    
    
    # 2.4 Create development and income index -----------------------------------------------------
    flog.info('Create development and income index')
    
    un_attr[(`More Developed Regions`), `:=`(development_index='More developed', development_color='#3A9400')]
    un_attr[(`Less Developed Regions`), `:=`(development_index='Less developed', development_color='#FF8E00')]
    un_attr[(`Least developed countries`), `:=`(development_index='Least developed', development_color='#C92918')]
    if(nrow(un_attr[is.na(development_index)])>0) flog.warn(paste('Found',nrow(un_attr[is.na(development_index)]), 
                                                              'countries without develop_index'))
    un_attr[is.na(development_index), `:=`(development_index='No info', development_color='#ffffff')]
    un_attr[,development_index:=factor(development_index, levels=c('More developed', 'Less developed', 'Least developed', 'No info'))]
    un_attr[,c('More Developed Regions', 'Less Developed Regions', 'Least developed countries'):=NULL]
    
    
    un_attr[(`High-income Countries`), `:=`(income_index='High-income', income_color='#3A9400')]
    un_attr[(`Upper-middle-income Countries`), `:=`(income_index='Upper-middle-income', income_color='#9cc382')]
    un_attr[(`Lower-middle-income Countries`), `:=`(income_index='Lower-middle-income', income_color='#ea947f')]
    un_attr[(`Low-income Countries`), `:=`(income_index='Low-income', income_color='#C92918')]
    if(nrow(un_attr[is.na(income_index)])>0) flog.warn(paste('Found',nrow(un_attr[is.na(income_index)]), 
                                                              'countries without income_index'))
    un_attr[is.na(income_index), `:=`(income_index='No info', income_color='#ffffff')]
    un_attr[,income_index:=factor(income_index, levels=c(  'High-income'
                                                         , 'Upper-middle-income'
                                                         , 'Lower-middle-income'
                                                         , 'Low-income'
                                                         , 'No info'))]
    un_attr[,c('High-income Countries', 'Low-income Countries', 'Middle-income Countries',
               'Upper-middle-income Countries', 'Lower-middle-income Countries', 'Sub-Saharan Africa'):=NULL]

    
    
    # 2.5 Defining colors -------------------------------------------------------------------------
    flog.info('Defining colors')
    
    #region_cols <- data.table(region=unique(un_attr$region), 
    #                          reg_color=brewer.pal(n = length(unique(un_attr$region)), name = "Set1"))
    region_cols <- data.table(region=unique(un_attr$region), reg_color='')
    region_cols[region=="EUROPE", reg_color:='#0A9FFF']
    region_cols[region=="NORTHERN AMERICA", reg_color:='#3A9400']
    region_cols[region=="LATIN AMERICA AND THE CARIBBEAN", reg_color:='#FF8E00']
    region_cols[region=="SUB-SAHARAN AFRICA", reg_color:='#00E17B']
    region_cols[region=="NORTHERN AFRICA AND WESTERN ASIA", reg_color:='#2A3F54']
    region_cols[region=="CENTRAL AND SOUTHERN ASIA", reg_color:='#C92918']
    region_cols[region=="EASTERN AND SOUTH-EASTERN ASIA", reg_color:='#FFE500']
    region_cols[region=="OCEANIA", reg_color:='#700FFF']
    
    setkey(un_attr, region)
    setkey(region_cols, region)
    un_attr <- un_attr[region_cols]
    un_attr[,region:=factor(region, levels=c("EUROPE"
                                             ,"NORTHERN AMERICA"
                                             ,"LATIN AMERICA AND THE CARIBBEAN"
                                             ,"SUB-SAHARAN AFRICA"
                                             ,"NORTHERN AFRICA AND WESTERN ASIA"
                                             ,"CENTRAL AND SOUTHERN ASIA"
                                             ,"EASTERN AND SOUTH-EASTERN ASIA"
                                             ,"OCEANIA"))]
    
    # 2.6 Writing formatted data ------------------------------------------------------------------
    flog.info('Writing formatted data')
    
    saveRDS(un_attr, '../data/un_country_attributes.rds')
  }
  
  # 3 Read formatted data -----------------------------------------------------------------------
  flog.info('Read formatted data')
  
  return(setDT(readRDS(file = '../data/un_country_attributes.rds')))
}

