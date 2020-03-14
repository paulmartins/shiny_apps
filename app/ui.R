library(shiny)
library(gentelellaShiny)
library(shinyWidgets)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(plotly)

options(shiny.jquery.version=1)

all_functions <- list.files('../functions/')
sapply(file.path('../functions', all_functions), source)

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
                          title="Guy Abel"
                        , ref="https://guyabel.com/post/animated-directional-chord-diagrams/"
                        , "Chord diagram article"
                        )
                    , notifItem(
                          title="Mark Edmondson"
                        , ref="http://code.markedmondson.me/gentelellaShiny/"
                        , "Shiny Gentellela theme"
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
                        , img="https://pngimg.com/uploads/un/un_PNG20.png"
                        , ref="https://www.un.org/en/development/desa/population/migration/data/estimates2/estimates19.asp"
                        , "External link to data source"
                        )
                    , notifItem(
                      title="OECD migration data"
                      , img="https://banner2.cleanpng.com/20180410/tpe/kisspng-oecd-d-8-organization-for-economic-cooperation-eco-economic-5acd14583c4cd3.555230361523389528247.jpg"
                      , ref="https://www.oecd.org/els/mig/keystat.htm"
                      , "External link to data source"
                      )
                    , notifItem(
                      title="Countries polygons"
                      , img="https://toppng.com/uploads/preview/if-you-live-in-country-outside-of-the-united-states-world-map-simple-transparent-background-11563161268uvzbxkn7ob.png"
                      , ref="https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/"
                      , "External link to data source"
                    )
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
                        , htmlOutput(outputId='map_title')
                        , prettyRadioButtons(
                              inputId='select_map_variable'
                            , label="Variable"
                            , inline=FALSE
                            , shape='curve'
                            , thick=TRUE
                            , choiceValues=main_map_radio_box_values
                            , choiceNames=radio_box_mapping(main_map_radio_box_values)
                            )
                        , chooseSliderSkin(skin="Modern", color='#73879C')
                        ,  sliderInput(
                            inputId="main_map_year"
                            , label="Year"
                            , min=1990
                            , max=2019
                            , value=2019
                            , step=5
                            , ticks= TRUE
                            , sep=""
                        )
                        , prettySwitch("legend", "Show legend", value=TRUE, slim=TRUE, status='primary')
                        , htmlOutput(outputId='migrant_notes')
                        )
                    , column(
                          width=9
                        , tags$style(type = "text/css", "#main_map {height: calc(100vh - 80px) !important;}")
                        , leafletOutput("main_map")
                        )
                )
                )
              
              # 3.2 World -------------------------------------------------------------------------
              
            , tabItem(
                  tabName="world"
                , fluidRow(
                    useShinyjs()
                    , column(
                          width=4
                        , align="left"
                        , htmlOutput(outputId='world_title')
                        , prettyRadioButtons(
                          inputId='select_chord_variable'
                          , label="Variable"
                          , choiceNames=radio_box_mapping(world_radio_box_values)
                          , choiceValues=world_radio_box_values
                          , shape='curve'
                          , thick=TRUE
                          , inline=FALSE
                        )
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
                        , prettySwitch("show_mini_map", "Show Mini Map", value=FALSE, slim=TRUE, status='primary')
                        , leafletOutput("mini_map", height = 275)
                        , htmlOutput(outputId='index_notes')
                        )
                    , column(
                          width=8
                        , align="left"
                        , tabsetPanel(
                              id='world_tabs'
                            , type="pills"
                            , shiny::tabPanel(
                                  "Migration flows"
                                , tabName="world_migr_flow"
                                , plotOutput("chord_diagram", height=700)
                                )
                            , shiny::tabPanel(
                                  "Demography"
                                , tabName="world_age_gender"
                                , plotlyOutput('gender_age_plot', height=700)
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

