library(shiny)
library(gentelellaShiny)
library(shinyWidgets)
library(shinyjs)

library(chorddiag)

options(shiny.jquery.version=1)



ui = gentelellaPageCustom(
    title = "UN Migration Dashboard",
    navbar = gentelellaNavbar(
        navbarItems = list(
            notif(
                id = "menunotif",
                icon = icon("envelope-o"),
                status = "primary",
                expanded = FALSE,
                lapply(X = 1:5, FUN = function(i) {
                    notifItem(
                        title = "John Doe",
                        date = "3 min ago",
                        img = paste0("https://image.flaticon.com/icons/svg/163/16382", i,".svg"),
                        "Film festivals used to be do-or-die moments"
                        )
                    })
            ),
            notif(
                id = "data_sources_link",
                icon = icon("info-circle"),
                status = "info",
                label_type = "",
                list(notifItem(title = "UN data source"),notifItem(title = "Country Polygons"))
            ),
            notif(
                id = "github_link",
                icon = icon("github"),
                label_type = "",
                status = "info",
                list(notifItem(title = "Country Polygons"))
            )
        )
    ),
    sidebar = gentelellaSidebar(
        tags$script(src="https://kit.fontawesome.com/6b09341d85.js"),
        site_title = p(icon("globe"), span("UN Migration")),
        uiOutput("profile"),
        sidebarDate(),
        sidebarMenu(
            sidebarItem("Maps", tabName = "tab1", icon = icon('map')),
            sidebarItem("World", tabName = "tab2", icon = tags$i(class = "fas fa-globe-europe")),
            sidebarItem("Country", tabName = "tab3", icon = tags$i(class = "fas fa-flag"))
        )
    ),
    body = gentelellaBody(
        tabItems(
            tabItem(
                tabName = "tab2",
                fluidRow(
                    column(
                        width = 4,
                        align = "left",
                        sliderInput(inputId="year", label = "Year", min=1990, max=2017, value=2017, 
                                    step=5, ticks= TRUE, sep='',
                                    animate = animationOptions(interval = 600, loop = FALSE, 
                                                               playButton = icon("play"), 
                                                               pauseButton = icon("pause"))
                                    ),
                        radioButtons('select_chord_variable', "Variable", inline = FALSE,
                                     choices = c("Development index", "Income index", "Region", "Sub-region"),
                                     selected = 'Development index'),
                        useShinyjs(),
                        disabled(
                            selectInput(inputId="region_sub_region", label="Region:", choices='')
                        )
                        ),
                    column(
                        width = 8,
                        align = "center",
                        chorddiagOutput("regionChord", height = 600)
                    )
                )
            ),
            tabItem(
                tabName = "tab3",
                jumbotron(
                    title = "Hello, world!",
                    "This is a simple hero unit, a simple jumbotron-style
        component for calling extra attention to featured
        content or information."
                )
            )
        )
    ),
    footer = gentelellaFooter(leftText = "Paul Martins, 2020", rightText = "Made in London, UK")
)

