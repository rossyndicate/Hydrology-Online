# Tab layout and sidebar panels for a shiny app that helps user
#See the biogeochemical impact of mountaintop mining to accompnay
#Ross et al., 2016


library(leaflet)
library(shiny)
library(dygraphs)
ions <- c('Ca', 'Mg', 'Na', 'K', 'SO4', 'HCO3', 'Cl', 'TDS')




shinyUI(fluidPage(
  sidebarLayout(
    #Setup condtional panel for when Elemental flux tab is not clicked. 
    sidebarPanel(
      width = 4,
      conditionalPanel(condition = "input.tabs != 'Elemental Flux'",
                       tabsetPanel(
                         tabPanel(
                           'Overview',
                           p(
                             "Mountaintop Mining has dramatically changed the physical and ecological landscape
                             of Central Appalachia. Mining activities flatten steep Appalachian mountains, fill valleys with
                             shattered bedrock and coal residues, and turn 2 meter deep soils into 100 meter deep spoils. Here
                             we show how these physical impacts to the landscape alter the hydrology and biogeochemistry
                             of two watersheds that have been heavily mined.
                             The study follows a paired watershed approach, where we have two reference watershed that are unmined paired with two
                             watersheds that have been mined. The small catchments (~1km2) are Rich's Branch (RB, reference) and
                             Laurel Branch (LB, 99% mined), while the large ones (35 km2) are Left Fork (LF, reference) and Mud River (MR, 46% mined).
                             To interact with the app click on a catchment and then select tabs.
                             For a deeper exploration of changes to hydrology please visit:",
                             a('MTM-HYDRO', href = "http://mtm-hydro.web.duke.edu"),
                             "For a broader look at how mining has changed watersheds throughout West Virginia please visit:",
                             a('MTM-GEOMORPH', href = 'http://mtm-explore.web.duke.edu')
                           ),
                           br(),
                           p(
                             em(
                               "This application was built by Matt Ross with support from NSF EAR, NSF GRFP"
                             )
                           )
                         ),
                         tabPanel('Map',
                                  leafletOutput("MapMine", height = 350)
                         )
                       )
      ),
      conditionalPanel(condition = "input.tabs == 'Elemental Flux'",
                       tabsetPanel(
                         tabPanel('Select Element',
                                  checkboxGroupInput(
                                    'Ion',
                                    label='Select ions to display',
                                    choices=ions,
                                    selected='Ca',inline=T
                                  ),
                                  conditionalPanel(
                                    condition="input.flx != 3",
                                    selectInput('gopt',
                                              'Graph Options',
                                              choices=list('Regular' =1,'Log Y'=2,'Relative to Total Flux'=3))),
                                  plotOutput('Flux',height='500px'))
                       )
      )
  ),
  
  
  #Setup conditional panel for when elemental flux is clicked. 
  
  mainPanel(
    width = 8,
    tabsetPanel(
      id = 'tabs',
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
          8,
          h5(
            'The top graph shows daily mean rainfall rates across all sites. The middle graph shows
              daily mean discharge and the bottom graph shows daily mean specific conductance, a proxy measure for
              salinity and total ion concentration. To change sites click on a new watershed on the map.
              You can zoom into each graph to look at specific time periods.'
          )
        ),
        column(
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
        )),
        
        br(),
        dygraphOutput('pplots', width = '95%', height =
                        '100px'),
        br(),
        dygraphOutput('qplots', width = '95%', height =
                        '200px'),
        br(),
        dygraphOutput('scplots', width = '95%', height =
                        '200px')
      ),
      tabPanel(
        "Elemental Flux",
        br(),
        fluidRow(column(
        #   6,
        #   selectInput(
        #     'Ion',
        #     label = 'Choose ion flux data to display',
        #     choices = ions,
        #     selected = 'Mg'
        #   )
        # ),
        # column(
          6,
          selectInput(
            'flx',
            label = 'Display options',
            choices = list(
              'Areal flux' = 1,
              'Total flux' = 2,
              'Concentration dynamics' = 3,
              'Stacked Total Flux' = 4
            ),
            selected = 1
          )
        )),
        br(),
        conditionalPanel(
          condition = "input.flx != 4",
          h4('1st Order Watersheds',align='center'),
          dygraphOutput('w1.q', width = '95%', height = '100px'), #1st order Q
          br(),
          dygraphOutput('w1.flux', width = '95%', height = '125px'), #1st Order dynamic flux
          h4('4th Order Watersheds',align='center'),
          dygraphOutput('w4.q',width='95%',height='100px'), #4th order Q
          br(),
          dygraphOutput('w4.flux',width='95%',height='125px') #4th order flux
        ),
        conditionalPanel(
          condition = "input.flx==4",
          dygraphOutput('rb.flx', width = '95%', height = '125px'),
          dygraphOutput('lb.flx', width = '95%', height = '125px'),
          dygraphOutput('lf.flx', width = '95%', height = '125px'),
          dygraphOutput('mr.flx', width = '95%', height = '125px')
        )
      )
    )
  )
)
))
