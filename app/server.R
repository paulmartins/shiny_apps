#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# download_raw_data()

un_country_yearly_age_attr <- setDT(readRDS(file='./data/un_country_yearly_age_attributes.rds'))
countries_poly <- readOGR(dsn=path.expand("./data/polygons/"), layer='countries')

server = function(input, output, session) {
    # 1 Maps --------------------------------------------------------------------------------------
    # 1.1 Reactives -------------------------------------------------------------------------------
    format_main_map_data <- reactive({
        map_data <- un_country_yearly_attr[year==input$main_map_year, .(code, var=get(input$select_map_variable), country)]
        map_data[, popup:=paste0(  '<span>'
                                   , sprintf('<img src="https://www.countryflags.io/%s/flat/24.png">'
                                             , tolower(countrycode(code, origin='un', destination='iso2c')))
                                   , "<strong><font color='#2A3F54'> "
                                   , country, '</font></strong></span>'
                                   , "<br>"
                                   , radio_box_mapping(input$select_map_variable)
                                   , ": <em>", var
                                   , ifelse(input$select_map_variable!='net_migration'," %</em>","</em>")
                                   )]
        map_data <- map_data[, .(code, var, popup)]
        map_data <- sp::merge(countries_poly, map_data, by.x='code', by.y='code')
        map_data
    })
    main_map_color_palette <- reactive({
        selected_bins <- switch(  input$select_map_variable
                                , "percent_migrant"=c(0, 2, 5, 10, 15, 20, 40, 100)
                                , "percent_f_migrant"=c(0, 40, 45, 48, 50, 52, 55, 100)
                                , "percent_refugees"=c(0, 0.1, 0.5, 1, 5, 10, 50, 1000)
                                , "net_migration"=c(-10000, -100, -50, -10, 0, 10, 50, 100, 10000))
        selected_palette <- switch(input$select_map_variable
                                   , "net_migration"=diverging_hue_colors(8)
                                   , single_hue_colors(7)
                                   )
        colorBin(  palette=selected_palette
                 , domain=unique(format_main_map_data()$var)
                 , bins = selected_bins
                 )
    })
    main_map_label_format <- reactive({
        switch(input$select_map_variable
               , "net_migration"=labelFormat(between=' &#10145; ', transform=function(x){x[which(x>500)] <- 500; x[which(x< -500)] <- -500; x})
               , labelFormat(suffix=' %', transform=function(x){x[which(x>100)] <- 100; x})
               )
    })
    # 1.2 Observers -------------------------------------------------------------------------------
    # Incremental changes to the map should be performed in an observer. 
    # Each independent set of things that can change should be managed in its own observer.
    observe({
        pal <- main_map_color_palette()
        leafletProxy("main_map", data = format_main_map_data()) %>%
            clearMarkerClusters() %>%
            clearShapes() %>%
            addPolygons(fillColor = ~pal(var),
                        fillOpacity = 1,
                        stroke = TRUE,
                        color = "#666",
                        dashArray = "2",
                        weight = 1,
                        group = "Polygons",
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#000",
                            dashArray = "",
                            fillOpacity = 0.75,
                            bringToFront = TRUE),
                        popup = ~popup) %>%
            addLayersControl(
                baseGroups = c("CartoDB", "OpenStreetMap", "Satellite"),
                overlayGroups = c("Polygons"),
                options = layersControlOptions(collapsed=TRUE)
            )
    })
    observe({
        proxy <- leafletProxy("main_map", data=format_main_map_data())
        proxy %>% clearControls()
        if (input$legend) {
            pal <- main_map_color_palette()
            proxy %>% addLegend( position="bottomleft"
                                , pal=pal
                                , values=~var
                                , opacity=1
                                , na.label="No info"
                                , title=radio_box_mapping(input$select_map_variable)
                                , labFormat=main_map_label_format()
            )
        }
    })
    # 1.3 Outputs ---------------------------------------------------------------------------------
    output$map_title <- renderText({'
        <h3>Maps explorer</h3>'
    })
    output$migrant_notes <- renderText({paste0('
        <strong>Notes on the metrics displayed</strong>
        <br>', radio_box_mapping(main_map_radio_box_values[1]), '
        <br><small><p>
        Total number of migrants in a country given as a percentage of its population at mid-year.</p></small>
        ', radio_box_mapping(main_map_radio_box_values[2]), '
        <br><small><p>
        Total number of female migrants in a country as a percentage of the total number of migrants.</p></small>
        ', radio_box_mapping(main_map_radio_box_values[3]), '
        <br><small><p>
        Total number of refugees (including asylum-seekers) in a country as a percentage of the total number of migrants</p></small>
        ', radio_box_mapping(main_map_radio_box_values[4]), '
        <br><small><p>
        Difference between immigrants and emigrants divided by the population at mid-year (more details 
        <a href="https://en.wikipedia.org/wiki/Net_migration_rate" target="_blank">here</a>)
        <br>')
    })
    output$main_map <- renderLeaflet({
        leaflet() %>% 
            setView(lng=0, lat=0, zoom=02) %>%
            addTiles(options=tileOptions(minZoom=01, maxZoom=16)) %>%
            addProviderTiles("CartoDB.Positron", group="CartoDB") %>%
            addProviderTiles("OpenStreetMap.Mapnik", group="OpenStreetMap") %>%
            addProviderTiles("Esri.WorldImagery", group="Satellite") 
    })
    
    # 2 World -------------------------------------------------------------------------------------
    # 2.1 Reactives -------------------------------------------------------------------------------
    format_mini_map_data <- reactive({
        mini_map_data <- un_country_attr[, .(  code
                                             , var=get(input$select_chord_variable)
                                             , country
                                             , flags_img
                                             )
                                         ]
        mini_map_data[, popup:=paste0(  '<span>'
                                      , sprintf('<img src="%s">',flags_img)
                                      , "<strong><font color='#2A3F54'> "
                                      , country, '</font></strong></span>'
                                      , "<br>"
                                      , radio_box_mapping(input$select_chord_variable)
                                      , ": <em>", var, "</em>")]
        mini_map_data <- mini_map_data[, .(code, var, popup)]
        mini_map_data <- sp::merge(countries_poly, mini_map_data, by.x='code', by.y='code')
        mini_map_data
    })
    mini_map_color_palette <- reactive({
        palette_table <- unique(un_country_attr[,.(var = get(input$select_chord_variable))])
        colorFactor(
            palette=get_chord_colors(variable=input$select_chord_variable, value=palette_table$var)
            , domain=palette_table$var
            , levels=palette_table$var
            , ordered=TRUE)
    })
    filtered_un_migration_by_year <- reactive({
        un_migration_flow[year == input$year]
    })
    filtered_un_country_yearly_age_by_year <- reactive({
        un_country_yearly_age_attr[year == input$year]
    })
    # 2.2 Observers -------------------------------------------------------------------------------
    observeEvent(input$show_mini_map, {
        shinyjs::toggle("mini_map", condition=input$show_mini_map)
    })
    observe({
        pal <- mini_map_color_palette()
        leafletProxy("mini_map", data = format_mini_map_data()) %>%
            clearMarkerClusters() %>%
            clearShapes() %>%
            addPolygons(fillColor = ~pal(var),
                        fillOpacity = 1,
                        color = "#666",
                        dashArray = "2",
                        weight = 1,
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#000",
                            dashArray = "",
                            fillOpacity = 0.75,
                            bringToFront = TRUE),
                        popup = ~popup)
    })
    # 2.3 Outputs ---------------------------------------------------------------------------------
    output$mini_map <- renderLeaflet({
        pal <- mini_map_color_palette()
        leaflet(data = format_mini_map_data()) %>% 
            setView(lng=0, lat=40, zoom=01) %>%
            addTiles(options=tileOptions(minZoom=01, maxZoom=16)) %>%
            addProviderTiles("CartoDB.Positron", group="CartoDB") %>%
            addPolygons(fillColor = ~pal(var),
                        fillOpacity = 1,
                        color = "#666",
                        dashArray = "2",
                        weight = 1,
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#000",
                            dashArray = "",
                            fillOpacity = 0.75,
                            bringToFront = TRUE),
                        popup = ~popup)
    })
    output$chord_diagram <- renderPlot({
        render_chord(  un_migration=filtered_un_migration_by_year()
                     , un_country_attr=un_country_attr
                     , variable=input$select_chord_variable)
    })
    output$gender_age_plot <- renderPlotly({
        render_age_gender(  age_data_plot=filtered_un_country_yearly_age_by_year()
                          , un_country_attr=un_country_attr
                          , variable=input$select_chord_variable)
    })
    output$world_title <- renderText({
        if(input$world_tabs=="Migration flows"){'
            <h3>International population migration flows</h3>
            <br>The chord diagram represents the international migration patterns as vectors from origin
            to destination by development index, income index or regions.
            The unit of the scale for the vectors widths is in million people.
            <br><br>'
        } else{
            '
            <h3>Demography</h3>
            <br>The pyramid graphs represent the number of male and female migrants in each age groups as a percentage 
            of the total international number of migrants by development index, income index or regions.
            <br><br>'
        }
    })
    output$index_notes <- renderText({'
        <br>
        <strong>Notes on the indexes</strong>
        <br>Development index:
        <br><small><p>
        The categories are intended for statistical convenience and do not necessarily express a 
        judgement about the stage reached by a particular country or area in the development process.</p></small>
        Income index:
        <br><small><p>
        The country classification by income level is based on June 2018 GNI per capita from the World Bank.</p></small>
        <br>'})
    
    # 3 Country -----------------------------------------------------------------------------------
    # 3.1 Reactives -------------------------------------------------------------------------------
    format_sankey_data <- reactive({
        un_data_graph_to <- un_migration_flow[year==input$country_year & country_to_code==input$main_country & value>0
                                                   , .(country_to, country_from, value)][order(value)]
        un_data_graph_from <- un_migration_flow[year==input$country_year & country_from_code==input$main_country & value>0
                                                     , .(country_to, country_from, value)][order(value)]
        rbind(un_data_graph_to, un_data_graph_from)
    })
    # 3.2 Observers -------------------------------------------------------------------------------
    observe({
        max_top_n_in <- format_sankey_data()[,.N,country_to][N>1]$N
        if(length(max_top_n_in)<1) max_top_n_in <- 0
        max_top_n_out <- format_sankey_data()[,.N,country_from][N>1]$N
        is_in_value_too_big <- input$top_n_in >= max_top_n_in
        is_out_value_too_big <- input$top_n_out >= max_top_n_out
        updateNumericInput(session, "top_n_in", max=max_top_n_in, value=ifelse(is_in_value_too_big, max_top_n_in, input$top_n_in))
        updateNumericInput(session, "top_n_out", max=max_top_n_out, value=ifelse(is_out_value_too_big, max_top_n_out, input$top_n_out))
        if(is_in_value_too_big){
            updatePrettySwitch(session, 'show_others_in', value = FALSE)
            shinyjs::disable('show_others_in')
        }
        else {
            shinyjs::enable('show_others_in')
        }
        if(is_out_value_too_big){
            updatePrettySwitch(session, 'show_others_out', value = FALSE)
            shinyjs::disable('show_others_out')
        } else{
            shinyjs::enable('show_others_out')
        }
    })

    # 3.3 Outputs ---------------------------------------------------------------------------------
    output$sankey_diagram <- renderSankeyNetwork({
        render_sankey(format_sankey_data()
                      , top_n_in=input$top_n_in
                      , top_n_out=input$top_n_out
                      , show_others_in=input$show_others_in
                      , show_others_out=input$show_others_out)
    })
    output$net_migration <- renderText({
        label_number_si(accuracy=0.01)(un_country_yearly_attr[year==input$country_year & code==input$main_country, net_migration])
    })
    output$net_migration_desc <- renderText({
        if(input$country_year == 1990){
            output <- ''
        } else {
            previous_value <- tail(un_country_yearly_attr[year<input$country_year & code==input$main_country, .(year, net_migration)][order(year)],1)
            current_value <- un_country_yearly_attr[year==input$country_year & code==input$main_country, net_migration]
            change_diff <- (current_value - previous_value$net_migration)
            if(change_diff>0){
                output <- sprintf('<p><font color="#1abb9c"> &#9650; %s </font> <em>since %s</em>', round(change_diff, 1), previous_value$year)
            }
            else if(change_diff<0){
                output <- sprintf('<p><font color="#de425b"> &#9660; %s </font> <em>since %s</em>', round(change_diff, 1), previous_value$year)
            }
            else{
                output <- sprintf('<p><font color="#2a3f54"> &#9654; %s </font> <em>since %s</em>', round(change_diff, 1), previous_value$year)
            }
        }
        output
    })
    output$total_immigrant <- renderText({
        label_number_si(accuracy=0.01)(un_country_yearly_attr[year==input$country_year & code==input$main_country, immigrants])
    })
    output$total_immigrant_desc <- renderText({
        if(input$country_year == 1990){
            output <- ''
        } else {
            previous_value <- tail(un_country_yearly_attr[year<input$country_year & code==input$main_country, .(year, immigrants)][order(year)],1)
            current_value <- un_country_yearly_attr[year==input$country_year & code==input$main_country, immigrants]
            change_diff <- label_number_si(accuracy=0.01)(current_value - previous_value$immigrants)
            if(change_diff>0){
                output <- sprintf('<p><font color="#1abb9c"> &#9650; %s </font> <em>since %s</em>', change_diff, previous_value$year)
            }
            else if(change_diff<0){
                output <- sprintf('<p><font color="#de425b"> &#9660; %s </font> <em>since %s</em>', change_diff, previous_value$year)
            }
            else{
                output <- sprintf('<p><font color="#2a3f54"> &#9654; %s </font> <em>since %s</em>', change_diff, previous_value$year)
            }
        }
        output
    })
    output$total_emigrant <- renderText({
        label_number_si(accuracy=0.01)(un_country_yearly_attr[year==input$country_year & code==input$main_country, emigrants])
    })
    output$total_emigrant_desc <- renderText({
        if(input$country_year == 1990){
            output <- ''
        } else {
            previous_value <- tail(un_country_yearly_attr[year<input$country_year & code==input$main_country, .(year, emigrants)][order(year)],1)
            current_value <- un_country_yearly_attr[year==input$country_year & code==input$main_country, emigrants]
            change_diff <- label_number_si(accuracy=0.01)(current_value - previous_value$emigrants)
            if(change_diff>0){
                output <- sprintf('<p><font color="#1abb9c"> &#9650; %s </font> <em>since %s</em>', change_diff, previous_value$year)
            }
            else if(change_diff<0){
                output <- sprintf('<p><font color="#de425b"> &#9660; %s </font> <em>since %s</em>', change_diff, previous_value$year)
            }
            else{
                output <- sprintf('<p><font color="#2a3f54"> &#9654; %s </font> <em>since %s</em>', change_diff, previous_value$year)
            }
        }
        output
    })
    output$total_pop <- renderText({
        label_number_si(accuracy=0.01)(un_country_yearly_attr[year==input$country_year & code==input$main_country, total_pop*1e06])
    })
    output$total_pop_desc <- renderText({
        if(input$country_year == 1990){
            output <- ''
        } else {
            previous_value <- tail(un_country_yearly_attr[year<input$country_year & code==input$main_country, .(year, total_pop)][order(year)],1)
            current_value <- un_country_yearly_attr[year==input$country_year & code==input$main_country, total_pop]
            change_diff <- label_number_si(accuracy=0.01)(1.e06*(current_value - previous_value$total_pop))
            if(change_diff>0){
                output <- sprintf('<p><font color="#1abb9c"> &#9650; %s </font> <em>since %s</em>', change_diff, previous_value$year)
            }
            else if(change_diff<0){
                output <- sprintf('<p><font color="#de425b"> &#9660; %s </font> <em>since %s</em>', change_diff, previous_value$year)
            }
            else{
                output <- sprintf('<p><font color="#2a3f54"> &#9654; %s </font> <em>since %s</em>', change_diff, previous_value$year)
            }
        }
        output
    })
    
    # 4 Tables ------------------------------------------------------------------------------------
    output$table_country_attr <- renderDataTable(
        datatable(un_country_attr[!is.na(country)
                                  , .(flag=sprintf('<img src="%s">',flags_img)
                                      , country, code, region, sub_region, development_index, income_index)
                                  , .(country, code)][
                                  , .(flag, country, code, region, sub_region, development_index, income_index)]
           , escape=1 # to render flags as html
           , filter='top'
           , extensions=c('Responsive', 'Scroller', 'Buttons')
           , options=list(
                 deferRender=TRUE
               , scrollY=700
               , scroller=TRUE
               , columnDefs=list(list(width='10px', targets=1))
               , dom='Bfrtip'
               , buttons=c('copy', 'csv', 'excel', 'print')
               , initComplete=JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': '#2a3f54', 'color': '#f1f1f1'});",
                   "}"
                   )
               )
           ) %>%
            formatStyle(  'region'
                        , color=styleEqual(  levels(un_country_attr$region)
                                           , region_colors(levels(un_country_attr$region))
                                           )
                        , fontWeight='bold'
                        )
        )
    
    output$table_country_yearly_attr <- renderDataTable(
        datatable(un_country_yearly_attr[!is.na(country)
                                         , .(country=as.factor(country)
                                             , code
                                             , year=as.factor(year)
                                             , percent_migrant=percent_migrant/100
                                             , percent_f_migrant=percent_f_migrant/100
                                             , total_refugees
                                             , total_pop)]
                  , filter='top'
                  , extensions=c('Responsive', 'Scroller', 'Buttons')
                  , options=list(
                        deferRender=TRUE
                      , scrollY=700
                      , scroller=TRUE
                      , columnDefs=list(list(width='10px', targets=1))
                      , dom='Bfrtip'
                      , buttons=c('copy', 'csv', 'excel', 'print')
                      , initComplete=JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#2a3f54', 'color': '#f1f1f1'});",
                          "}"
                          )
                      )
                  ) %>%
            formatStyle(  c('percent_migrant', 'percent_f_migrant')
                          , target='cell'
                          , background=styleColorBar(data=c(0,1), color='#c8e5ff')
                          , backgroundSize = '100% 75%'
                          , backgroundRepeat = 'no-repeat'
                          , backgroundPosition = 'center'
                          )  %>%
        formatPercentage(c('percent_migrant', 'percent_f_migrant'))
        )
    output$table_country_yearly_age_attr <- renderDataTable(
        datatable(un_country_yearly_age_attr[!is.na(country)
                                         , .(country=as.factor(country)
                                             , code
                                             , year=as.factor(year)
                                             , age_group=as.factor(age_group)
                                             , migrant_male
                                             , migrant_female
                                             , pop_male
                                             , pop_female)]
                  , filter='top'
                  , extensions=c('Responsive', 'Scroller', 'Buttons')
                  , options=list(
                      deferRender=TRUE
                      , scrollY=700
                      , scroller=TRUE
                      , columnDefs=list(list(width='10px', targets=1))
                      , dom='Bfrtip'
                      , buttons=c('copy', 'csv', 'excel', 'print')
                      , initComplete=JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#2a3f54', 'color': '#f1f1f1'});",
                          "}"
                      )
                  )
        ) 
    )
    output$table_migration_flow <- renderDataTable(
        datatable(un_migration_flow[!is.na(country_from) & !is.na(country_to) & !is.na(value)
                                    , .(  year=as.factor(year)
                                        , country_to=as.factor(country_to)
                                        , country_from=as.factor(country_from)
                                        , value)]
                  , filter='top'
                  , extensions=c('Responsive', 'Scroller', 'Buttons')
                  , options=list(
                      deferRender=TRUE
                      , scrollY=700
                      , scroller=TRUE
                      , columnDefs=list(list(width='10px', targets=1))
                      , dom='Bfrtip'
                      , buttons=c('copy', 'csv', 'excel', 'print')
                      , initComplete=JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#2a3f54', 'color': '#f1f1f1'});",
                          "}"
                      )
                  )
        ) 
    )
}
    
    
    
    