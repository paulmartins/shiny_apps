#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

all_functions <- list.files('../functions/')
sapply(file.path('../functions', all_functions), source)

un_migration_flow <- fread('../data/un_migration_flow.csv')
un_country_attr <- fread('../data/un_country_attributes.csv')
un_country_attr <- calculate_chord_max(un_country_attr, un_migration_flow)

countries_poly <- readOGR(dsn=path.expand("../data/polygons/"), layer='countries_complete')

server = function(input, output, session) {
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            setView(lng=-0.127758, lat=51.507351, zoom=02) %>%
            addTiles() %>%
            addSearchOSM() %>%
            addTiles(options=tileOptions(minZoom=02, maxZoom=16)) %>%
            addProviderTiles("CartoDB.Positron", group="CartoDB") %>%
            addProviderTiles("OpenStreetMap.Mapnik", group="OpenStreetMap") %>%
            addProviderTiles("Esri.WorldImagery", group="Satellite") 
    })
    
    # Reactive expression for the data subsetted to what the user selected
    format_map_data <- reactive({
        # grab variable column name, colors and max values from the input variable arg
        var <- tolower(gsub(pattern="[\\s|-]", "_", input$select_map_variable, perl=TRUE))
        selected_color <- switch(  var
                                   , "income_index"='income_color'
                                   , "development_index"='development_color'
                                   , "reg_color")

        un_country_attr[, .(code, var=get(var), col=get(selected_color))][match(countries_poly$code, un_country_attr$code)]
    })
    
    colorpal <- reactive({
        if(input$select_map_variable %in% c("Development index", "Income index", "Region")){
            var <- tolower(gsub(pattern="[\\s|-]", "_", input$select_map_variable, perl=TRUE))
            selected_color <- switch(  var
                                     , "income_index"='income_color'
                                     , "development_index"='development_color'
                                     , "reg_color")
            palette_table <- unique(un_country_attr[, .(var=get(var), col=get(selected_color))])
            palette <- colorFactor(  palette=palette_table$col
                                   , domain=palette_table$col
                                   , levels=palette_table$var)
        }
        palette
    })
    
    # Incremental changes to the map should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        pal <- colorpal()
        leafletProxy("map", data = countries_poly) %>%
            clearMarkerClusters() %>%
            clearShapes() %>%
            addPolygons(fillColor = pal(format_map_data()$var),
                        fillOpacity = 0.75,
                        stroke = TRUE,
                        color = "#666",
                        dashArray = "2",
                        weight = 1,
                        group = "Polygons",
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#000",
                            dashArray = "",
                            fillOpacity = 1.0,
                            bringToFront = TRUE)) %>%
            addLayersControl(
                baseGroups = c("CartoDB", "OpenStreetMap", "Satellite"),
                overlayGroups = c("Polygons"),
                options = layersControlOptions(collapsed = TRUE)
            )
    })
    # Use a separate observer to recreate the legend as needed.
    observe({
        proxy <- leafletProxy("map", data = countries_poly)
        proxy %>% clearControls()
        if (input$legend) {
            pal <- colorpal()
            proxy %>% addLegend( position="bottomleft"
                                , pal=pal
                                , values = format_map_data()$var
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