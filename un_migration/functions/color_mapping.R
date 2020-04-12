
development_index_colors <- function(idx_category){
  sapply(as.character(idx_category), USE.NAMES=FALSE, function(category){
    switch( category
           ,'More developed'  = '#1abb9c'
           ,'Less developed'  = '#007c8f'
           ,'Least developed' = '#2a3f54'
           ,'#ffffff') # last is default for any other non matching index like NA
  })
}

income_index_colors <- function(idx_category){
  sapply(as.character(idx_category), USE.NAMES=FALSE, function(category){
    switch( category
           ,'High-income'         = '#1abb9c'
           ,'Upper-middle-income' = '#a1decc'
           ,'Lower-middle-income' = '#9099a6'
           ,'Low-income'          = '#2a3f54'
           ,'#ffffff') # last is default for any other non matching index like NA
    })
}

region_colors <- function(region){
  sapply(as.character(region), USE.NAMES=FALSE, function(reg){
    switch( reg
           ,'EUROPE'                           = '#79c725'
           ,'NORTHERN AMERICA'                 = '#4daecf'
           ,'LATIN AMERICA AND THE CARIBBEAN'  = '#3a57bf'
           ,'SUB-SAHARAN AFRICA'               = '#7328b6'
           ,'NORTHERN AFRICA AND WESTERN ASIA' = '#dd3371'
           ,'CENTRAL AND SOUTHERN ASIA'        = '#f2823a'
           ,'EASTERN AND SOUTH-EASTERN ASIA'   = '#f6c137'
           ,'OCEANIA'                          = '#ffff2d'
           ,'#ffffff') # last is default for any other non matching index like NA
  })
}

single_hue_colors <- function(n){
  colorRampPalette(c('#c8e5ff', "#2A3F54"))(n)
}
diverging_hue_colors <- function(n){
  colorRampPalette(c('#de425b', '#f1f1f1', "#488f31"))(n)
}

get_chord_colors <- function(variable, values){
  FUN <- switch( variable
                 ,'development_index'=development_index_colors
                 ,'income_index'=income_index_colors
                 ,'region'=region_colors)
  FUN(values)
}

