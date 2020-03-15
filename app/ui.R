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

un_migration_flow <- fread('../data/un_migration_flow.csv')
un_country_attr <- setDT(readRDS(file='../data/un_country_attributes.rds'))
un_country_yearly_attr <- fread('../data/un_country_yearly_attributes.csv')


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
                    , notifItem(
                          title="Flags API"
                        , ref="https://www.countryflags.io/"
                        , "Free API to get any country's flag"
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
        , site_title=p(icon("globe"), span("Dashboard"))
        , sidebarDate()
        , sidebarMenu(
              sidebarItem("Home", tabName="home", icon=icon('home'))
            , sidebarItem("Maps", tabName="maps", icon=icon('map'))
            , sidebarItem("World", tabName="world", icon=tags$i(class="fas fa-globe-europe"))
            , sidebarItem("Country", tabName="country", icon=tags$i(class = "fas fa-flag"))
            , sidebarItem("Tables", tabName="table", icon=icon('table'))
            )
        )
    
    # 3 Main body panel ---------------------------------------------------------------------------
    
    , body=gentelellaBody(
      
        tabItems(
          
              # 3.1 Home --------------------------------------------------------------------------
              
              tabItem(
                  tabName="home"
                , jumbotron(title="International Migration Dashboard"
                            , "Quickly visualize the international population migration data"
                            , button=FALSE)
                , hr()
                , fluidRow(
                    column(
                        width=3
                      , panel_div(  class_type = "default"
                                  , panel_title = 'Map Explorer'
                                  , icon=tags$i(class="far fa-map")
                                  , content = "Switch between different country-level migration metrics and watch their evolution for the past 30 years"
                                  )
                    )
                  , column(
                      width=3
                    , panel_div(  class_type = "default"
                                , panel_title = 'World Statistics'
                                , icon=tags$i(class="fas fa-globe-europe")
                                , content = "Demographic metrics and migration flows aggregated at the flollowing levels:
                                              <ul>
                                                <li><a href='https://unstats.un.org/sdgs/report/2019/regional-groups/'>SDG</a> regions</li>
                                                <li>Development index</li>
                                                <li>Income index</li>
                                              </ul>"
                                )
                    )
                  , column(
                    width=3
                    , panel_div(  class_type = "default"
                                  , panel_title = 'Country Statistics'
                                  , icon=tags$i(class="fas fa-flag")
                                  , content = "A more detailed view of the net migration rate for each country"
                    )
                  )
                  , column(
                    width=3
                    , panel_div(  class_type = "default"
                                  , panel_title = 'Tables'
                                  , icon=tags$i(class="fas fa-table")
                                  , content = "Formatted data tables used to make all the charts"
                    )
                  )
                )
              )
          
              # 3.2 Maps --------------------------------------------------------------------------
              
            , tabItem(
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
              
              # 3.3 World -------------------------------------------------------------------------
              
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
                          , label="Group by Variable"
                          , choiceNames=radio_box_mapping(world_radio_box_values)
                          , choiceValues=world_radio_box_values
                          , selected=world_radio_box_values[3]
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
                        , prettySwitch("show_mini_map", "Show map legend", value=TRUE, slim=TRUE, status='primary')
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
            
            # 3.4 Country -----------------------------------------------------------------------
            
            , tabItem(
                  tabName="country"
                , fluidRow(
                    column(
                        width=2
                      , align="left"
                      , pickerInput(  inputId="main_country"
                                    , label="Country"
                                    , choices=un_country_attr[order(country)]$code
                                    , multiple=FALSE
                                    , selected=826
                                    , choicesOpt=list(content=sprintf('<span><img src="%s"> %s</span>'
                                                                      , un_country_attr[order(country)]$flags_img
                                                                      , un_country_attr[order(country)]$country))
                                    , options = list(`live-search`=TRUE)
                                    )
                      , sliderInput(  inputId="country_year"
                                    , label="Year"
                                    , min=1990
                                    , max=2019
                                    , value=2019
                                    , step=5
                                    , ticks=FALSE
                                    , sep=""
                                    )
                      )
                  , column(
                      width=10
                    , dashboard_box(
                        width='100%'
                      , height=220
                      , box_title="Statisics"
                      , list(total_pop=valueBox(textOutput('total_pop') 
                                                      , width=4
                                                      , description=htmlOutput('total_pop_desc') 
                                                      , title='Total population'
                                                      , icon=icon('male')
                                                      )
                             ,total_immigrant=valueBox(textOutput('total_immigrant') 
                                                    , width=4
                                                    , description=htmlOutput('total_immigrant_desc') 
                                                    , title='Immigrants'
                                                    , icon=icon('compress-arrows-alt')
                                                    )
                             ,total_emigrant=valueBox(textOutput('total_emigrant') 
                                                     , width=4
                                                     , description=htmlOutput('total_emigrant_desc') 
                                                     , title='Emigrants'
                                                     , icon=icon('expand-arrows-alt')
                             )
                             ,net_migration_rate=valueBox(  textOutput('net_migration') 
                                                         , width=4
                                                         , description=htmlOutput('net_migration_desc') 
                                                         , title='Net migration rate'
                                                         , icon=icon('exchange-alt')
                                                         )
                             )
                      )
                  )
                )
                , fluidRow(
                    column(
                      width=2
                    , numericInput(  inputId="top_n_in"
                                   , label="No. origin countries"
                                   , value=10
                                   , min=0
                                   , max=30
                                   , step=1
                                   )
                    , prettySwitch("show_others_in", "Show Others", value=TRUE, slim=TRUE, status='primary')
                    , numericInput(  inputId="top_n_out"
                                   , label="No. destination countries"
                                   , value=10
                                   , min=0
                                   , max=30
                                   , step=1
                                   )
                    , prettySwitch("show_others_out", "Show Others", value=TRUE, slim=TRUE, status='primary')
                    )
                  , column(
                      width=10
                    , align="left"
                    , sankeyNetworkOutput("sankey_diagram", height=700)
                    )
                  )
              )
            
              # 3.5 Table -------------------------------------------------------------------------
            
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

