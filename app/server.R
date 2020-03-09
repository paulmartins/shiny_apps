#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


un_data <- fread('../data/un_migration.csv')
un_attr <- fread('../data/un_attributes.csv')
un_attr <- calculate_chord_max(un_attr, un_data_original=un_data)

source('../render_chord2.R')


server = function(input, output, session) {
    
    # Reactive expression for the data subsetted to what the user selected
    filtered_un_data_year <- reactive({
        un_data[year == input$year,]
    })
    
    # reactive department to populate options in ui filter
    sub_region_filter <- reactive({
        input$select_chord_variable
    })
    
    observeEvent(sub_region_filter(), {
        if(sub_region_filter() == "Sub-region") {
            enable("region_sub_region")
            updateSelectInput(session, "region_sub_region", choices = unique(un_attr$region))
        }
    })
    
    output$regionChord <- renderPlot({
        render_chord(un_data=filtered_un_data_year(), 
                     un_attr, 
                     variable=input$select_chord_variable)
    })
    output$chord_title <- renderText({'
        <h3>International population migration flows</h3>
        <br>The chord diagram represents the international migration patterns as vectors from origin 
        to destination by development index, income index or regions.
        The unit of the scale for the vectors widths is in million people.
        <br><br>'
        })
    output$index_notes <- renderText({'
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
    
    counter <- reactiveValues(connect = 0)
    
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