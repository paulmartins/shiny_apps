library(shiny)
library(gentelellaShiny)
library(shinyWidgets)
library(shinyjs)
library(leaflet)
library(leaflet.extras)

options(shiny.jquery.version=1)

source('../gentelella_updates.R')

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
                id = "credits",
                icon = icon("thumbs-up"),
                status = "info",
                label_type = "",
                list(notifItem(title = "Guy Abel chord digram article",
                               ref = "https://guyabel.com/post/animated-directional-chord-diagrams/"),
                     notifItem(title = "Mark Edmondson's Shiny theme",
                               ref = "http://code.markedmondson.me/gentelellaShiny/")
                )
            ),
            notif(
                id = "data_sources_link",
                icon = icon("info-circle"),
                status = "info",
                label_type = "",
                list(notifItem(title = "UN migration data",
                               img = "https://cdn.freebiesupply.com/logos/large/2x/un-1-logo-png-transparent.png",
                               ref = "www.unmigration.org",
                               "International migrant stock"),
                     notifItem(title = "Country Polygons"))
            ),
            notif(
                id = "github_link",
                icon = icon("github"),
                label_type = "",
                status = "info",
                list(notifItem(title = "Dashboard source code",
                               img = "https://www.nicepng.com/png/full/183-1838159_exploring-github-github-octocat.png",
                               "github.com/paulmartins/un_migration",
                               ref = "https://github.com/paulmartins/un_migration"))
            )
        )
    ),
    sidebar = gentelellaSidebar(
        tags$script(src="https://kit.fontawesome.com/6b09341d85.js"),
        site_title = p(icon("globe"), span("UN Migration")),
        uiOutput("profile"),
        sidebarDate(),
        sidebarMenu(
            sidebarItem("Maps", tabName = "maps", icon = icon('map')),
            sidebarItem("World", tabName = "world", icon = tags$i(class = "fas fa-globe-europe")),
            sidebarItem("Country", tabName = "country", icon = tags$i(class = "fas fa-flag"))
        )
    ),
    body = gentelellaBody(
        tabItems(
            tabItem(
                tabName = "maps",
                fluidRow(
                    column(
                        width = 8,
                        #leafletOutput("map")
                    )
                )
            ),
            tabItem(
                tabName = "world",
                fluidRow(
                    tabsetPanel(
                        type = "pills",
                        shiny::tabPanel("Migration flows",
                                        tabName="world_migr_flow",
                                        tags$style(HTML(" .nav {margin-bottom:20px;}")),
                                        column(
                                            width = 4,
                                            align = "left",
                                            htmlOutput(outputId='chord_title'),
                                            sliderInput(inputId="year", label = "Year", 
                                                        min=1990, max=2019, value=2019, 
                                                        step=5, ticks= TRUE, sep='',
                                                        animate = animationOptions(interval = 900, loop = FALSE, 
                                                                                   playButton = icon("play"), 
                                                                                   pauseButton = icon("pause")
                                                                                   )
                                                        ),
                                            radioButtons(inputId='select_chord_variable', "Variable", inline = FALSE,
                                                         choices = c("Development index", "Income index", "Region"),
                                                         selected = 'Development index'),
                                            htmlOutput(outputId='index_notes')
                                            ),
                                        column(
                                            width = 8,
                                            align = "left",
                                            plotOutput("regionChord", height = 700)
                                            )
                                        ),
                        shiny::tabPanel("Age and Gender", 
                                        tabName="world_age_gender",
                                        tags$style(HTML(" .nav {margin-bottom:20px;}")))
                        )
                    )
                ),
            tabItem(
                tabName = "country",
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

