library(shiny)
library(gentelellaShiny)
library(shinyWidgets)
library(shinyjs)
library(leaflet)
library(leaflet.extras)

options(shiny.jquery.version=1)


ui = gentelellaPageCustom(
      title="UN Migration Dashboard"
      
      # 1 Top bavigation bar ----------------------------------------------------------------------
      
    , navbar=gentelellaNavbar(
        navbarItems=list(
              notif(
                  id="credits"
                , icon=icon("thumbs-up")
                , status="info"
                , label_type=""
                , list(
                      notifItem(
                          title="Guy Abel chord digram article"
                        , ref="https://guyabel.com/post/animated-directional-chord-diagrams/"
                        )
                    , notifItem(
                          title="Mark Edmondson's Shiny theme"
                        , ref="http://code.markedmondson.me/gentelellaShiny/"
                        )
                    )
                )
            , notif(
                  id="data_sources_link"
                , icon=icon("info-circle")
                , status="info"
                , label_type=""
                , list(
                      notifItem(
                          title="UN migration data"
                        , img="https://cdn.freebiesupply.com/logos/large/2x/un-1-logo-png-transparent.png"
                        , ref="www.unmigration.org"
                        , "International migrant stock"
                        )
                    , notifItem(
                        title="Country Polygons")
                    )
                )
            , notif(
                  id="github_link"
                , icon=icon("github")
                , label_type=""
                , status="info"
                , list(
                    notifItem(
                        title="Dashboard source code"
                        , img="https://www.nicepng.com/png/full/183-1838159_exploring-github-github-octocat.png"
                        , ref="https://github.com/paulmartins/un_migration"
                        , "github.com/paulmartins/un_migration"
                        )
                    )
                )
            )
        )
    
    # 2 Left side navigation bar ------------------------------------------------------------------
    
    , sidebar=gentelellaSidebar(
          tags$script(src="https://kit.fontawesome.com/6b09341d85.js")
        , site_title=p(icon("globe"), span("UN Migration Dashboard"))
        , sidebarDate()
        , sidebarMenu(
              sidebarItem("Maps", tabName="maps", icon=icon('map'))
            , sidebarItem("World", tabName="world", icon=tags$i(class="fas fa-globe-europe"))
            , sidebarItem("Country", tabName="country", icon=tags$i(class = "fas fa-flag"))
            , sidebarItem("Tables", tabName="table", icon=icon('table'))
            )
        )
    
    # 3 Main body panel ---------------------------------------------------------------------------
    
    , body=gentelellaBody(
        tabItems(
              # 3.1 Maps --------------------------------------------------------------------------
              tabItem(
                  tabName="maps"
                , fluidRow(
                      column(
                          width=3
                        , align="left"
                        , radioButtons(
                              inputId='select_map_variable'
                            , label="Variable"
                            , inline=FALSE
                            , choices=c("Development index", "Income index", "Region", 
                                        "Number of migrants (% total population)",
                                        "Number of females migrants (% of migrants)",
                                        "Number of refugees (% total population)")
                            , selected='Development index'
                            )
                        ,  sliderInput(
                            inputId="map_year"
                            , label="Year"
                            , min=1990
                            , max=2019
                            , value=2019
                            , step=5
                            , ticks= TRUE
                            , sep=""
                        )
                        , prettySwitch("legend", "Show legend", value=TRUE, slim=TRUE, status='primary')
                        )
                    , column(
                          width=9
                        , tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
                        , leafletOutput("map")
                        )
                )
                )
              # 3.2 World -------------------------------------------------------------------------
            , tabItem(
                  tabName="world"
                , fluidRow(
                    column(
                          width=4
                        , align="left"
                        , htmlOutput(outputId='chord_title')
                        , sliderInput(
                            inputId="year"
                            , label="Year"
                            , min=1990
                            , max=2019
                            , value=2019
                            , step=5
                            , ticks= TRUE
                            , sep=""
                            , animate=animationOptions(
                                  interval=900
                                , loop=FALSE
                                , playButton=icon("play")
                                , pauseButton = icon("pause")
                                )
                            )
                        , radioButtons(
                              inputId='select_chord_variable'
                            , label="Variable"
                            , inline=FALSE
                            , choices=c("Development index", "Income index", "Region")
                            , selected='Development index'
                            )
                        , htmlOutput(outputId='chord_index_notes')
                        )
                    , column(
                          width=8
                        , align="left"
                        , tabsetPanel(
                              type="pills"
                            , shiny::tabPanel(
                                  "Migration flows"
                                , tabName="world_migr_flow"
                                , plotOutput("chord_diagram", height=700)
                                )
                            , shiny::tabPanel(
                                  "Age and Gender"
                                , tabName="world_age_gender"
                                )
                            )
                        )
                    )
            )
              # 3.3 Country -----------------------------------------------------------------------
            , tabItem(
                  tabName="country"
                , jumbotron(
                    title="Hello, world!",
                    "This is a simple hero unit, a simple jumbotron-style
                    component for calling extra attention to featured
                    content or information."
                    )
                )
              # 3.4 Table -------------------------------------------------------------------------
            , tabItem(
                tabName="table"
                )
            )
        )
    , footer=gentelellaFooter(
          leftText="Paul Martins, 2020"
        , rightText="Made in London, UK"
        )
    )

