#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


un_migration_flow <- fread('../data/un_migration_flow.csv')
un_country_attr <- setDT(readRDS(file = '../data/un_country_attributes.rds'))
un_country_attr <- calculate_chord_max(un_country_attr, un_migration_flow)
un_country_yearly_attr <- fread('../data/un_country_yearly_attributes.csv')
countries_poly <- readOGR(dsn=path.expand("../data/polygons/"), layer='countries_complete')

server = function(input, output, session) {
    output$map <- renderLeaflet({
        leaflet() %>% 
            setView(lng=0, lat=0, zoom=02) %>%
            addTiles(options=tileOptions(minZoom=01, maxZoom=16)) %>%
            addProviderTiles("CartoDB.Positron", group="CartoDB") %>%
            addProviderTiles("OpenStreetMap.Mapnik", group="OpenStreetMap") %>%
            addProviderTiles("Esri.WorldImagery", group="Satellite") 
    })
    
    # Reactive expression for the data subsetted to what the user selected
    format_map_data <- reactive({
        if(!input$select_map_variable %in% c("development_index", "income_index", "region")){
            map_data <- un_country_yearly_attr[year==input$map_year]}
        else{
            map_data <- un_country_attr
        }
        map_data <- map_data[, .(code, var=get(input$select_map_variable))]
        map_data <- sp::merge(countries_poly, map_data, by.x='code', by.y='code')
        # reorder in the same as polygons
        # map_data[match(countries_poly$code, map_data$code)]
        map_data
    })
    
    colorpal <- reactive({
        if(input$select_map_variable %in% c("development_index", "income_index", "region")){
            palette_table <- unique(un_country_attr[,.(var = get(input$select_map_variable))])
            palette <- colorFactor(  palette=get_chord_colors(variable=input$select_map_variable, value=palette_table$var)
                                   , domain=palette_table$var
                                   , levels=palette_table$var
                                   , ordered=TRUE)
        } else{
            selected_bins <- switch(  input$select_map_variable
                                     , "percent_migrant"=c(0,1,5,10,20,100)
                                     , "percent_f_migrant"=c(0,40,45,50,55,100)
                                     , "percent_refugees"=c(0,0.15,0.5,1,5,100))
            palette <- colorBin(  palette=single_hue_colors(5)
                                     , domain=unique(format_map_data()$var)
                                     , bins = selected_bins
                                     )
        }
        palette
    })
    
    # Incremental changes to the map should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        pal <- colorpal()
        leafletProxy("map", data = format_map_data()) %>%
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
                        popup =  ~paste0(
                            "<strong><font color='#2A3F54'>",country,"</font></strong>", 
                            "<br>",input$select_map_variable,": <em>",var,"</em>")) %>%
            addLayersControl(
                baseGroups = c("CartoDB", "OpenStreetMap", "Satellite"),
                overlayGroups = c("Polygons"),
                options = layersControlOptions(collapsed = TRUE)
            )
    })
    # Use a separate observer to recreate the legend as needed.
    observe({
        proxy <- leafletProxy("map", data = format_map_data())
        proxy %>% clearControls()
        if (input$legend) {
            pal <- colorpal()
            proxy %>% addLegend( position="bottomleft"
                                , pal=pal
                                , values = ~var
                                , opacity = 1
                                , title = input$select_map_variable
                                , na.label = "No info"
            )
        }
    })
    
    # Reactive expression for the data subsetted to what the user selected
    filtered_un_migration_by_year <- reactive({
        un_migration_flow[year == input$year]
    })
    
    output$chord_diagram <- renderPlot({
        render_chord(  un_migration=filtered_un_migration_by_year()
                     , un_country_attr=un_country_attr
                     , variable=input$select_chord_variable)
    })
    output$chord_title <- renderText({'
        <h3>International population migration flows</h3>
        <br>The chord diagram represents the international migration patterns as vectors from origin 
        to destination by development index, income index or regions.
        The unit of the scale for the vectors widths is in million people.
        <br><br>'
        })
    output$chord_index_notes <- renderText({'
        <br>To know which countries are included in the indexes categories, check out the map layers.
        <br><br>
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