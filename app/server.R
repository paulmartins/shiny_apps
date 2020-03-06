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

render_chord <- function(un_data, un_attr, variable = 'region', sub_region_filter=NULL){
    variable <- tolower(gsub(pattern = '[\\s|-]', '_', variable, perl=TRUE))
    
    # merge variable from attribute into migration data
    setkey(un_data, country_to_code)
    setkey(un_attr, code)
    un_data <- un_data[un_attr[,.(code, var_to=get(variable))]]
    
    setkey(un_data, country_from_code)
    un_data <- un_data[un_attr[,.(code, var_from=get(variable))]]
    
    if(variable == 'sub_region'){
        subregion_in_region <- unique(un_attr[region == sub_region_filter, sub_region])
        plot_data <- un_data[var_to %in% subregion_in_region & var_from %in% subregion_in_region,.(value=sum(value, na.rm=TRUE)),.(var_to, var_from)][value >0]
    } else{
        plot_data <- un_data[,.(value=sum(value, na.rm=TRUE)),.(var_to, var_from)][value >0]
    }
    
    plot_data <- dcast(plot_data, formula = var_to ~ var_from)
    var_to <- plot_data$var_to
    
    plot_data[, var_to := NULL]
    var_from <- names(plot_data)
    
    plot_matrix <- as.matrix(plot_data)
    dimnames(plot_matrix) <- list(var_to = var_to, var_from = var_from)
    
    chorddiag(plot_matrix, groupnamePadding = 20, groupnameFontsize = 10, showTicks = FALSE, tooltipGroupConnector = " from ")
    #groupColors=c('#8E44AD', '#27AE60', '#2980B9', '#F39C12', '#F1C40F', '#E74C3C')
}


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
    
    output$regionChord <- renderChorddiag({
        render_chord(un_data=filtered_un_data_year(), un_attr, 
                     variable=input$select_chord_variable, 
                     sub_region_filter=input$region_sub_region)
    })
    
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