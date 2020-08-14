

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(leaflet)
library(shiny)


shinyUI(fluidPage(theme = "bootstrap.css",
                  sidebarLayout(
                    sidebarPanel(
                      p(
                        "This application shows how mining has changed the slope and elevation
                        profiles of landscapes in West Virginia, along with providing an estimate of
                        total spoil volume deposited into headwater valleys as a result of mining. The number
                        that shows up when you click on a watershed is an estimate of how deeply mine spoil would bury
                        the watershed if the spoil were spread evenly across it."
                      ),
                      radioButtons(
                        "ES",
                        label = 'Choose Data to Explore',
                        choices = list(
                          'Changes in slope' = 1,
                          'Changes in elevation' =
                            2
                        ),
                        selected = 1
                      ),
                      leafletOutput("MapMine", height = 350),
                      p(
                        em(
                          "This application was built with support from Duke University's
                          Data+ program with help from Molly Rosenstein, Tess Harper, and Aaron Berdanier"
                        )
                        ),
                      width = 4
                      ),
                    mainPanel(width = 8,
                              tabsetPanel(
                                tabPanel(
                                  "Map View",
                                  fluidRow(column(
                                    6,
                                    checkboxInput("PrePost",
                                                  label =
                                                    'Toggle pre- or post-mining data',
                                                  value =
                                                    T)
                                  ),
                                  column(
                                    6,
                                    checkboxInput('Diff', label = 'Toggle elevation change map',
                                                  value = F)
                                  )),
                                  uiOutput('ID'),
                                  fluidRow(column(12,
                                                  imageOutput('rastplots')))
                                ),
                                tabPanel("Plot View", plotOutput('gplots')),
                                tabPanel(
                                  "Volume of Spoil",
                                  plotOutput('barplot', height = "350px"),
                                  plotOutput('secondbar', height = "250px")
                                )
                              ))
                    )))
