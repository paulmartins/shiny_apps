#' @description Creates a set of shapefiles matching UN data countries in /data/polygons
#' @param un_attr un_country_attr data.table created by the function \code{create_un_country_attr}

# un_attr <- setDT(readRDS('../data/un_country_attributes.rds'))

create_countries_polygons <- function(un_attr){
  

  # 1 Formatting polygons attributes -----------------------------------------------------------
  flog.info('Formatting polygons attributes')
  
  countries_poly <- readOGR(dsn=path.expand("../data_raw/polygons/"), layer = 'CNTR_RG_60M_2016_4326')
  countries_poly$CNTR_NAME <- countrycode(unique(countries_poly$ISO3_CODE),'iso3c','country.name')
  countries_poly$UN_CODE <- countrycode(unique(countries_poly$ISO3_CODE),'iso3c','un')
  countries_poly <- countries_poly[!is.na(countries_poly$CNTR_NAME),]
  countries_poly$NAME_ENGL <- NULL
  countries_poly$FID <- NULL
  countries_poly$CNTR_ID <- NULL

  
  # 2 Matching countries polygon to un data -------------------------------------------------------
  
  flog.info(paste( 'Removing polygons that are not in UN data:'
                   , countries_poly[!countries_poly$UN_CODE %in% un_attr$code,]$CNTR_NAME)
  )
  countries_poly <- countries_poly[countries_poly$UN_CODE %in% un_attr$code,]
  
  flog.info(paste(  'Finding missing polygons for:'
                    , un_attr[!code %in% countries_poly$UN_CODE, country])
  )
  
  missing_loc <- data.table(  code=un_attr[!code %in% countries_poly$UN_CODE, code]
                            , country=un_attr[!code %in% countries_poly$UN_CODE,country]
                            , long=0
                            , lat=0)
  
  
  # 3 Add here each country/region that didn't get a matching polygon -----------------------------
  # Get location from https://www.latlong.net/
  
  missing_loc[country=='Mayotte', `:=`(long=45.166245, lat=-12.827500)]
  missing_loc[country=='French Guiana', `:=`(long=-52.335049, lat=4.938000)]
  missing_loc[country=='Guadeloupe', `:=`(long=-61.550999, lat=16.264999)]
  missing_loc[country=='Martinique', `:=`(long=-61.024174, lat=14.641528)]
  missing_loc[country=='Réunion', `:=`(long=55.450020, lat=-20.882980)]
  missing_loc <- missing_loc[long != 0 & lat != 0]
  missing_loc = st_as_sf(missing_loc, coords=c("long", "lat"), crs=4326, agr="constant")
  
  
  # 4 Find a polygon matching the missing locations -----------------------------------------------
  flog.info('Find a polygon matching the missing locations')
  
  # transforms sp to st countries polygons
  countries_poly_sf <- st_as_sf(countries_poly)
  # find countries that interescts with the missing loc
  idx_country_intersect <- as.numeric(st_intersects(missing_loc, countries_poly_sf))
  # split each country into its list of polygons
  country_split_polygons <- st_cast(countries_poly_sf[idx_country_intersect,], "POLYGON")
  # find which polygon ech point intersects with
  idx_poly_intersect <- as.data.table(st_intersects(  missing_loc[, 'geometry']
                                                    , country_split_polygons[, 'geometry']))
  idx_poly_intersect <- idx_poly_intersect[, .(poly_id=min(col.id, na.rm=TRUE)), row.id][order(row.id)]
  # removing point geom to insert polygon geom
  missing_loc$geometry <- NULL
  missing_loc$polygon <- country_split_polygons[idx_poly_intersect$poly_id, 'geometry']
  # convert to multi-polygon to bind the data.table
  missing_loc <- st_as_sf(missing_loc) %>% st_cast('MULTIPOLYGON')
  
  
  # 5 Removing matched polygons form old multipolygons --------------------------------------------
  flog.info('Removing matched polygons form old multipolygons')
  
  # put first the new one, so that the later indices will be cleared from the new polygons
  all_polygons <- rbindlist(list(
    as.data.table(missing_loc)[, .(  country
                                     , iso3=countrycode(  code
                                                          , origin='un'
                                                          , destination='iso3c')
                                     , code
                                     , geometry=polygon
    )]
    , as.data.table(countries_poly_sf)[, .(CNTR_NAME, ISO3_CODE, UN_CODE, geometry)]
  ))
  
  # necessary to avoid error in st_difference
  flog.info("Making sure the geom are valid (this might take a while)")
  all_polygons <- lwgeom::st_make_valid(st_as_sf(all_polygons))
  
  flog.info("Recalculate the countries polygons (this might take a while)")
  all_new_polygons <- st_difference(all_polygons)
  
  # 6 Writing formatted polygons ------------------------------------------------------------------
  flog.info('Writing formatted polygons')
  
  new_countries_poly <- as_Spatial(all_new_polygons)
  writeOGR(new_countries_poly, '../data/polygons', "countries", driver="ESRI Shapefile", overwrite_layer=TRUE)
}
