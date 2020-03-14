#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# download_raw_data()

un_migration_flow <- fread('../data/un_migration_flow.csv')
un_country_attr <- setDT(readRDS(file='../data/un_country_attributes.rds'))
un_country_yearly_attr <- fread('../data/un_country_yearly_attributes.csv')
un_country_yearly_age_attr <- setDT(readRDS(file='../data/un_country_yearly_age_attributes.rds'))

countries_poly <- readOGR(dsn=path.expand("../data/polygons/"), layer='countries')


server = function(input, output, session) {
    # 1 Maps --------------------------------------------------------------------------------------
    # 1.1 Reactives -------------------------------------------------------------------------------
    format_main_map_data <- reactive({
        map_data <- un_country_yearly_attr[year==input$main_map_year, .(code, var=get(input$select_map_variable), country)]
        map_data[, popup:=paste0(
            "<strong><font color='#2A3F54'>", country,"</font></strong>", 
            "<br>", radio_box_mapping(input$select_map_variable), ": <em>", var, " %</em>")]
        map_data <- map_data[, .(code, var, popup)]
        map_data <- sp::merge(countries_poly, map_data, by.x='code', by.y='code')
        map_data
    })
    main_map_color_palette <- reactive({
        selected_bins <- switch(  input$select_map_variable
                                     , "percent_migrant"=c(0, 2, 5, 10, 15, 20, 40, 100)
                                     , "percent_f_migrant"=c(0, 40, 45, 48, 50, 52, 55, 100)
                                     , "percent_refugees"=c(0, 0.1, 0.5, 1, 5, 10, 50, 1000))
        colorBin(  palette=single_hue_colors(7)
                 , domain=unique(format_main_map_data()$var)
                 , bins = selected_bins
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
                options = layersControlOptions(collapsed = TRUE)
            )
    })
    observe({
        proxy <- leafletProxy("main_map", data = format_main_map_data())
        proxy %>% clearControls()
        if (input$legend) {
            pal <- main_map_color_palette()
            proxy %>% addLegend( position="bottomleft"
                                , pal=pal
                                , values = ~var
                                , opacity = 1
                                , na.label = "No info"
                                , title = radio_box_mapping(input$select_map_variable)
                                , labFormat = labelFormat(suffix = ' %'
                                                          ,transform = function(x){x[which(x>100)] <- 100; x})
            )
        }
    })
    # 1.3 Outputs ---------------------------------------------------------------------------------
    output$map_title <- renderText({'
        <h3>Maps explorer</h3>'
    })
    output$migrant_notes <- renderText({paste0('
        <strong>Notes on the migrants metrics</strong>
        <br>', radio_box_mapping(main_map_radio_box_values[1]), ':
        <br><small><p>
        Total number of migrants in a country given as a percentage of its population at mid-year.</p></small>
        ', radio_box_mapping(main_map_radio_box_values[2]), ':
        <br><small><p>
        Total number of female migrants in a country as a percentage of the total number of migrants.</p></small>
        ', radio_box_mapping(main_map_radio_box_values[2]), ':
        <br><small><p>
        Total number of refugees (including asylum-seekers) in a country as a percentage of the total number of migrants</p></small>
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
        mini_map_data <- un_country_attr[, .(code, var=get(input$select_chord_variable), country)]
        mini_map_data[, popup:=paste0(
                "<strong><font color='#2A3F54'>", country,"</font></strong>", 
                "<br>", radio_box_mapping(input$select_chord_variable), ": <em>", var, "</em>")]
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
        toggle("mini_map")
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
            <br>The pyramid graphs represent the number of male and female migrants by age group as a percentage 
            of the total international number of migrants.
            <br><br>'
        }
    })
    output$index_notes <- renderText({'
        <strong>Notes on the indexes</strong>
        <br>Development index:
        <br><small><p>
        The categories are intended for statistical convenience and do not necessarily express a 
        judgement about the stage reached by a particular country or area in the development process.</p></small>
        Income index:
        <br><small><p>
        The country classification by income level is based on June 2018 GNI per capita from the World Bank.</p></small>
        <br>'})
}