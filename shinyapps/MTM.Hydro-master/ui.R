# Tab layout and sidebar panels for a shiny app that helps user
#See the hydrological impact of mountaintop mining to accompnay
#Nippgen et al., 2016


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(leaflet)
library(shiny)
library(dygraphs)


shinyUI(fluidPage(sidebarLayout(
  sidebarPanel(
    p(
      "This application shows how mountaintop mining in Central Appalachia has changed
      the hydrology and water chemistry of two catchments that have been heavily mined. The study follows a paired
      watershed approach, where we have two reference catchment that are unmined paired with two
      catchments that have been mined. The small catchments (~1km2) are Rich's Branch (reference) and
      Laurel Branch (99% mined), while the large ones (35 km2) are Left Fork (reference) and Mud River.
      To interact with the app click on a catchment and then select tabs. "
    ),
    leafletOutput("MapMine", height = 350),
    p(
      em(
        "This application was built by Matt Ross and Fabian Nippgen with support from NSF EAR"
      )
    ),
    width = 4
    ),
  mainPanel(width = 8,
            tabsetPanel(
              #Bit of code to containerize the gif plot
              
              tabPanel(
                "Geomorphology",
                textOutput('Sum.Text'),
                tags$head(
                  tags$style(type = "text/css",
                             "#geogif img {max-width: 100%; width: 100%; height: auto}")
                ),
                imageOutput('geogif')
              ),
              tabPanel(
                "Hydrology and Specific Conductance",
                br(),
                fluidRow(column(
                    4,
                    selectInput(
                      'comp',
                      label = 'Compare Selected Catchment With:',
                      choices = list(
                        'Mined vs Unmined' = 1,
                        '1st vs 4th Order streams' = 2,
                        'Single Watershed' = 3
                      ),
                      selected = 3
                    )
                  ),column(
                  8,
                  h5(
                    'The top graph shows total daily rainfall. The middle graph shows
                    total daily discharge and the bottom graph shows daily mean specific conductance, a proxy measure for
                    salinity and total ion concentration. Bottom graphs show cumulative discharge trends.  To change sites click on a new watershed on the map.
                    You can zoom into each graph to look at specific time periods.'
                  )
                  )),
                
                br(),
                dygraphOutput('pplots', width = '95%', height =
                                '100px'),
                br(),
                dygraphOutput('qplots', width = '95%', height =
                                '125px'),
                br(),
                dygraphOutput('scplots', width = '95%', height =
                                '125px'),
                br(),
                plotOutput('cume.plot',width='95%',height='200px')
                ),
              tabPanel(
                "Baseflow",
                br(),
                fluidRow(column(
                  5,
                  selectInput(
                    'base',
                    label = 'Choose baseflow data display',
                    choices = list('Compare baseflow between sites' = 1,
                                   'Flow separation at each site' = 2),
                    selected=2
                  )),
                  column(7,
                         h5("Baseflow was separated from stormflow using methods following",
                            a("Hewlett and Hibbert 1966.", href="http://coweeta.uga.edu/publications/pubs_martha_new_01282003/Batch_1_@300dpi/PDF/851.pdf"),
                         "This method
                            assumes a linear rise in baseflow during storms, which is why you will see linear baseflow increases
                            during large or successive storms")
                )),
                br(),
                conditionalPanel(
                  condition = "input.base == 1",
                dygraphOutput('p.base', width = '95%', height = '100px'),
                dygraphOutput('q1.base', width = '95%', height = '200px'),
                dygraphOutput('q4.base', width = '95%', height = '200px')
                ),
                conditionalPanel(
                  condition="input.base==2",
                  dygraphOutput('b1', width = '95%', height = '125px'),
                  dygraphOutput('b2', width = '95%', height = '125px'),
                  dygraphOutput('b3', width = '95%', height = '125px'),
                  dygraphOutput('b4', width = '95%', height = '125px')
                )
              )
            ))
  )))
