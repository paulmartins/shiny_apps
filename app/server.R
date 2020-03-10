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

un_migration_flow <- fread('../data/un_migration.csv')
un_country_attr <- fread('../data/un_attributes.csv')
un_country_attr <- calculate_chord_max(un_country_attr, un_migration_flow)

countries_poly <- readOGR(dsn=path.expand("../data/polygons/"), layer = 'countries_complete')


server = function(input, output, session) {
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>%
            addSearchOSM() %>%
            addTiles(group="OSM (default)") %>%
            addProviderTiles("CartoDB.Positron", group="CartoDB") #%>%
            #addProviderTiles("Esri.WorldImagery", group="Satellite")
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
    
    counter <- reactiveValues(connect=0)
    
    observeEvent(counter$connect == 0, {
        inputSweetAlert(
            session = session, 
            inputId = "name",
            title = "What's your name ?"
        )
    })
    
    output$profile <- renderUI({
        sidebarProfile(
            name = input$name,
            img = "https://image.flaticon.com/icons/svg/236/236831.svg"
        )
    })
}