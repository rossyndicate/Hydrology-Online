# A shiny web app to explore the Discharge record of gaged streams at HBEF
# J.P. Gannon
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(EcoHydRology)
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(patchwork)
library(readr)
library(grid)



# Define UI
ui <- fluidPage(
             
                #set theme for app
                theme = shinytheme("lumen"),
                
                #title
                titlePanel("HBEF Discharge"),
                
                sidebarLayout(
                  sidebarPanel(
                    h4("In this app you can explore Precipitation and Streamflow in Several HBEF watersheds"),
                
                    h5(strong("TO ZOOM: On the streamflow plot, click and drag and then double click. 
                      Double click again to zoom to full extent.")),
                    # Select type data to plot
                    selectInput(inputId = "type", label = strong("Streamflow plot units:"),
                                choices = c("Discharge (L/s)","Discharge (mm/day)"),
                                selected = "Discharge (L/s)"),
                    
                    selectInput("WSs", label = strong("Select watersheds to plot:"), 
                                choices = c("ws1","ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), 
                                multiple = TRUE, selected = c("ws3","ws9")),
                    
                    
                    # Select date range to be plotted
                    #dateRangeInput("date", strong("Date range"), start = "2015-08-01", end = "2019-02-12",
                     #              min = "2015-08-01", max = "2019-2-12"),
                    
                    # Select whether to log primary axis
                    checkboxInput(inputId = "log", label = strong("Log streamflow y axis."), value = FALSE),
                    
                    # Select to show calculated baseflow, and choose filter parameter
                    checkboxInput(inputId = "bf", label = strong("Show calculated baseflow. (mm/d only)"), value = FALSE),
                    numericInput(inputId = "filter", label = "Filter parameter for baseflow calculation", value = 0.999, min = 0, max = 1),
                    
                    #add a horizontal line to the plot to facilitate data comparisons
                    numericInput(inputId = "horizLine", label = strong("Add a horizontal line to Discharge plot at:"), 
                                 value = 0, min = 0, max = 100),
                    
                    #Identiy range of precip to highlight
                    sliderInput("Prange", label = strong("Range of Precip Values to Highlight"), value = c(0, 150), min = 0, max = 100),
                    
                    #change the size of the text in the plot using a slider
                    sliderInput(inputId = "mag", label = strong("Plot text size."), min = 11, max = 24, value = 12),
                    
                    #download button
                    # Button
                    #downloadButton("downloadData", "Download Selected Data"),
                    
                    # Pop up box if user clicks "Data Information"
                    actionButton("DataInfo","Data Information"),
                    #info
                    tags$div("Built by:", a(href = "http://www.jpgannon.com", "JP Gannon"), tags$br(), "Collegiate Assistant Professor", tags$br(), "Virginia Tech")#,
                   # a(href = "https://github.com/jpgannon/...", "Source Code"),
                    
                  ),
                  
                  # Output: Set up plot panel
                  mainPanel(
                    plotOutput(outputId = "precip", height = "150px"),
                    plotOutput(outputId = "lineplot", height = "250px",
                               dblclick = "plot1_dblclick",
                               brush = brushOpts(
                                 id = "plot1_brush",
                                 resetOnNew = TRUE)),
                    plotOutput(outputId = "therest", height = "250px"),
                    dataTableOutput("table")
                  )
                  
                ))


# Define server function
server <- function(input, output) {
  #setwd("/Volumes/GoogleDrive/My Drive/HBEF Discharge App/HBEFapp_to_send")
  
  # Pop up box if user clicks "Data Information"
  observeEvent(input$DataInfo, {
    showModal(modalDialog(
      title = "Data Information",
      HTML("<b> Discharge Data </b> <br> 
                       
              <a href=https://portal.lternet.edu/nis/mapbrowse?scope=knb-lter-hbr&identifier=1>LTER Data Portal</a>
                        </br>
                        USDA Forest Service, Northern Research Station 2019. Hubbard Brook Experimental 
                        Forest: Instantaneous Streamflow by Watershed, 1956 – present. Environmental 
                        Data Initiative. https://doi.org/10.6073/pasta/282953c2290b1f00d9326ffd9a7e9668.</br></br>
              <b> Precipitation Data </b> <br>
                       <a href=https://portal.lternet.edu/nis/mapbrowse?scope=knb-lter-hbr&identifier=14>LTER Data Portal</a>
                       </br>
                        USDA Forest Service, Northern Research Station 2019. Hubbard Brook Experimental
                        Forest: Total Daily Precipitation by Watershed, 1956 - present. Environmental 
                        Data Initiative. https://doi.org/10.6073/pasta/c64ad38eef4f56d9e34749f166f64caa. 
                             ")
    ))
  })
  
  #set default data ranges
  ranges <- reactiveValues(x = as.POSIXct(c(start = "2013-01-01", end = "2017-10-08")))
  maxrange <- reactiveValues(x = as.POSIXct(c(start = "2013-01-01", end = "2017-10-08")))
  
  #read discharge data
  Q <- read_csv("allwatersheds_hourly.csv") 
  
  #grab watershed names from data
  WSeds <- unique(Q$WS)
  
  #Remove na's for data to go to baseflow calculation
  Q_narm <- Q %>% drop_na(Discharge_ls)
  
 
  #read precip data
  P <- read_csv("dailyWatershedPrecip1956-2019.csv") %>% 
    filter(watershed == 3) %>%
    mutate(Highlight = NA)
  
  Pcalc <- read_csv("dailyWatershedPrecip1956-2019.csv")
  
  #run baseflow separation 
 QBF <- reactive({ if(input$bf){
   
   req(input$filter)
  
   Q_narm <- Q_narm %>% filter(WS %in% input$WSs)
   
    QBF <- Q_narm %>%
      group_by(WS) %>%
      group_map(~ BaseflowSeparation(.x$Discharge_mmd, input$filter)) %>%
      set_names(input$WSs) %>%
      bind_rows(.id = "WS") %>%
      bind_cols(Q_narm)
    }
   QBF
   })
 
 #calculate total discharge and precip for the plotted period
 totals <- reactive({
   PT <- Pcalc %>% filter(between(as.POSIXct(DATE), ranges$x[1], ranges$x[2])) %>%
    group_by(watershed) %>%
    summarise(TotalP = round(sum(Precip, na.rm = TRUE),2))
   
   QT <- Q %>% filter(between(DATETIME, ranges$x[1], ranges$x[2])) %>%
     group_by(WS) %>%
     summarise(TotalQ = round(sum(Discharge_mmd/24, na.rm = TRUE),2)) #mmd to mm/5min
   
  bind_cols(PT, QT) %>% filter(WS %in% input$WSs) %>%
    select(-WS) %>% 
    mutate(RR = round(TotalQ/TotalP, 2)) %>%
    rename('Total Q (mm)' = TotalQ, 'Total P (mm)' = TotalP, 'Runoff Ratio' = RR)
  
  })
 
 #filter discharge between selected dates
 Qsub <- reactive({
   Qsub <- Q %>% filter(between(DATETIME, ranges$x[1], ranges$x[2]) & WS %in% input$WSs)
   Qsub
 })
 
 #create precip plot
  output$precip <- renderPlot({
    
    P <- P %>% mutate(Highlight = ifelse(Precip >= input$Prange[1] & Precip <= input$Prange[2], Precip, NA))
    
    P %>% filter(between(as.POSIXct(DATE), ranges$x[1], ranges$x[2])) %>%
      ggplot(aes(DATE, Precip))+
      geom_bar(stat = "identity")+
      scale_y_reverse()+
      theme_classic()+
      geom_bar(aes(DATE, Highlight), fill = "blue", stat = "identity")+
      ylab("Precip (mm)")+
      xlab(element_blank())+
      theme(text = element_text(size=input$mag))+
      theme(plot.margin = unit(c(5.5,5.5,5.5,20), "pt"))
    })
  
  # Create discharge plot
  output$lineplot <- renderPlot({

    if(input$type == "Discharge (L/s)") {
      plot1 <- Qsub() %>%
        ggplot(aes(DATETIME, Discharge_ls, color = WS))+
        geom_line()+
        ylab(input$type)+
        xlab(element_blank())+
        theme_classic()+
        theme(text = element_text(size=input$mag))+
        geom_hline(yintercept = input$horizLine, color = "red")+
        theme(plot.margin = unit(c(5.5,5.5,5.5,15), "pt"))+
        theme(legend.position="bottom")
        
    }
    
    if(input$type == "Discharge (mm/day)") {
      plot1 <- Qsub() %>% 
        ggplot(aes(DATETIME, Discharge_mmd, color = WS))+
        geom_line()+
        ylab(input$type)+
        xlab(element_blank())+
        theme_classic()+
        theme(text = element_text(size=input$mag))+
        geom_hline(yintercept = input$horizLine, color = "red")+
        theme(plot.margin = unit(c(5.5,5.5,5.5,15), "pt"))+
        theme(legend.position="bottom")
    }
    
    #log axis
    if(input$log) plot1 <- plot1 + scale_y_log10()  
    
    #change y axis units
    if(input$bf & input$type == "Discharge (mm/day)"){
      QBFsub <- QBF() %>% filter(between(DATETIME, ranges$x[1], ranges$x[2]))
      plot1 <- plot1 + geom_line(data = QBFsub, mapping = aes(DATETIME, bt, color = WS), linetype = 2)
    }

    plot1
     })
  
  #create flow duration plots
  output$therest <- renderPlot({
    
    QsubECDF <- Qsub() %>%
      ggplot(aes(Discharge_mmd, color = WS))+
      stat_ecdf(geom = "line")+
      scale_y_continuous(labels = function(x) 1-x)+
      scale_x_log10(labels = function(x) format(x, scientific = FALSE))+
      coord_flip()+
      ylab("Exceedance Probability")+
      xlab("Discharge (mm/d)")+
      theme(legend.position = "bottom")+
      theme(text = element_text(size=input$mag))+
      theme_classic()+
      theme(legend.position='none')+
      theme(text = element_text(size=input$mag))+
      ggtitle("Selected Record")

    QECDF <- Q %>% filter(WS %in% input$WSs) %>%
      ggplot(aes(Discharge_mmd, color = WS))+
      stat_ecdf(geom = "line")+
      scale_y_continuous(labels=function(x)1-x)+
      scale_x_log10(labels = function(x) format(x, scientific = FALSE))+
      coord_flip()+
      ylab("Exceedance Probability")+
      xlab("Discharge (mm/d)")+
      theme(legend.position = "bottom")+
      theme(text = element_text(size=input$mag))+
      theme_classic()+
      theme(legend.position='none')+
      theme(text = element_text(size=input$mag))+
      ggtitle("Whole Record")

      QECDF + QsubECDF

  })
  
  #output table with total precip and discharge
  output$table <- renderDataTable(
    totals()
  )
  
  # Downloadable csv of selected dataset ----
  # output$downloadData <- downloadHandler(
  #   filename = "selectedHBEFdata.csv",
  #   content = function(file) {
  #     write.csv(Qsub(), file, row.names = FALSE)
  #   }
  # )
  
  #interactive plot subsetting with brushing
   observeEvent(input$plot1_dblclick,
               {
                 brush <- input$plot1_brush
                 if (!is.null(brush)) 
                 {
                   ranges$x <- c(brush$xmin, brush$xmax)
                   
                 } else {
                   ranges$x <- maxrange$x
                   }
               })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

