#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#Load libraries
library(shiny)
library(leaflet)
library(dygraphs)
vars <-
  sort(c(
    'Chloride',
    "Cadmium",
    "Calcium",
    "Iron",
    "Manganese",
    "Magnesium",
    "pH",
    "Selenium",
    "Zinc",
    "Specific conductance",
    "Sulfur, sulfate (SO4) as SO4",
    "Copper",
    "Lead",
    "Sodium",
    "Phosphate-phosphorus as P",
    "Inorganic nitrogen (nitrate and nitrite) as N",
    "Aluminum"
  ))               


# Define User Interface
shinyUI(fluidPage(sidebarLayout(
  sidebarPanel(
    p('In this teaching module, you will explore changing water quality in the 
      Snake River Watershed (near Keystone Ski Area, in red) and Upper Blue River Watershed (near Breckenridge, in blue). 
      We will look at a 50-year record of concentration of key nutrients, pollutants, and discharge,
      to gain a better understanding of the factors (land use, discharge, pH)
      that influence water quality in high mountain watersheds. Markers on map show location of WQ monitoring sites.
      Code, a lesson summary, and data for this web application can be found at my',
      a('Github site',href='https://github.com/matthewross07/WR440.Lesson')
    ),
    leafletOutput('map',height=350) #Sets dimensions of leaflet map
  ),
    mainPanel(
      tabsetPanel(
        tabPanel('WQ change over time',
                 fluidRow(column(4,
      selectInput('analyte',label='Select a water quality analyte',
                  choices=vars)),
      column(4,
             p('Look up drinking water thresholds',
               
               a('Primary',href='https://www.epa.gov/ground-water-and-drinking-water/national-primary-drinking-water-regulations'),
               'or',
               a('Secondary',href='https://www.epa.gov/dwstandardsregulations/secondary-drinking-water-standards-guidance-nuisance-chemicals')
             )
      ),
      column(4,
             numericInput('thresh',label='Enter a Regulation Threshold',value=NA))
      ),
      plotOutput('diff',height='160px'),
      h4('Analyte Concentration'),
      dygraphOutput('time.chem',height='160px'),
      h4('Daily Mean Discharge'),
      dygraphOutput('q',height='160px')
      ),
    tabPanel('WQ change with flow and season',
             fluidRow(column(
               4,
             selectInput('analyte1',label='Select an analyte',
                         choices=vars)),
             column(4,
                    selectInput('model',label='Select a model to fit',
                                choices=c('none','yx','logx','logy','logyx'))),
             column(4,
                    selectInput('season',label='Select a season',
                                choices=c('all','summer','winter')))),
             br(),
             h3('Daily Discharge'),
             dygraphOutput('q2', height='200px'),
             h3('QC plots'),
             plotOutput('chemostasis',height='200px')
             )
    )
))))
