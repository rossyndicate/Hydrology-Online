#Shiny Server


#Load packages
library(magicaxis)
library(shiny)
library(leaflet)
library(ggplot2)
library(sp)
options(scipen = 10)

#ggplot2 theme changes

#ggplot  theme  changes
matt_theme <- theme_set(theme_bw())
matt_theme <-
  theme_update(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title = element_text(face = 'bold'),
    text = element_text(
      family = 'sans',
      face = 'plain',
      colour = 'black',
      size = 14,
      hjust = .5,
      vjust = .5,
      angle = 0,
      lineheight = 0
    )
  )


load('Data/All.Dat.RData')


#Setup shiny server
shinyServer(function(input, output) {
  #Setup Leaflet Map
  
  output$MapMine <- renderLeaflet({
    #Setup color ramp
    mapramp <- colorRampPalette(c('#fee0d2', '#67000d'))(10)
    #Feed color ramp into Leaflet color ramp scheme.
    pals.huc <-
      colorQuantile(
        palette = mapramp,
        domain = hucMine$VF_Depth,
        n = 5,
        alpha = F
      )
    pals.county <-
      colorQuantile(
        palette = mapramp,
        domain = CountyCoal$VF_Depth,
        n = 5,
        alpha = F
      )
    leaflet() %>% addProviderTiles("Esri.WorldTopoMap", group = 'Topo Map') %>%
      addProviderTiles('Esri.WorldImagery', group = 'Aerial Imagery') %>%
      #providerTileOptions(attribution='ESRI World Topo Map') %>%
      #   addRasterImage(r) %>% addRasterImage(r1) %>% addRasterImage(r2) %>%
      #   addRasterImage(r3) %>% addRasterImage(r4) %>%
      addPolygons(
        data = hucMine,
        weight = .5,
        smooth = 0,
        stroke = T,
        fillOpacity = 0.6,
        color = pals.huc(hucMine$VF_Depth),
        popup = paste('Burial depth ', round(hucMine$VF_Depth, 2), " m", sep =
                        ''),
        layerId = hucMine$HUC12,
        group = 'Watersheds'
      ) %>%
      addLegend(
        position = 'bottomright',
        values = hucMine$VF_Depth,
        pal = pals.huc,
        title = 'Rank Impact'
      ) %>%
      
      addLayersControl(
        baseGroups = c('Topo Map', 'Aerial Imagery'),
        overlayGroups = c('Watersheds'),
        options = layersControlOptions(collapsed = F, autoZIndex = T)
      )
    
  })
  
  
  id <- reactive({
    validate(
      need(
        input$MapMine_shape_click != "",
        "Please select a watershed or county from the map to the left to view plots and data.
        App may take a few seconds to load data after selecting data (depending on internet connection speed)."
      )
      )
    (input$MapMine_shape_click)
  })
  
  # Stupid approach to get rid of double erros but it shoudl work
  id1 <- reactive({
    validate(need(input$MapMine_shape_click != "", ""))
    (input$MapMine_shape_click)
  })
  
  # Generate volume plots based on user input
  observe({
    output$ID <- renderUI({
      h5(names[id1()$id == names$id, 'name'], align = 'center')
    })
    if (id()$group == 'Watersheds') {
      output$barplot <- renderPlot({
        par(cex = 1.2)
        dat <-
          hucMine$SUM[which(hucMine$HUC12 == as.character(id()$id))] * 100
        hgts <- dat / areas$val
        hgts[hgts < 0.1] <- 0.1
        barplot(
          height = hgts,
          horiz = F,
          log = "y",
          ylab = 'Burial depth (m)',
          names.arg = areas$names,
          las = 1,
          col = "darkgrey",
          ylim = c(0.1, 100000),
          yaxt = 'n',
          main = bquote(paste(
            .(round(dat / (
              1000 * 1000 * 1000
            ), 3)), km ^ 3, " of Spoil Spread Over Various Areas"
          ))
        )
        magaxis(2,
                majorn = 3,
                minorn = 10,
                cex.axis = 1)
        #text(((dat/areas$val)/4)+3,y= (1:nrow(areas))*1.2, labels=(areas$names), pos=1, font= 2)
      })
      output$secondbar <- renderPlot({
        dat <-
          hucMine$SUM[which(hucMine$HUC12 == as.character(id()$id))] * 100
        barplot(height = 6.4,
                horiz = T,
                xlim = c(0, 7))
        par(new = T)
        pct <- round(dat / (6.4 * 1000 * 1000 * 10), 1)
        barplot(
          height = dat / (1000 * 1000 * 1000),
          xlim = c(0, 7),
          xaxt = 'n',
          col = 'darkred',
          horiz = T,
          main = 'Contribution of Watershed to Total Spoil Volume',
          xlab = expression(paste('Volume (', km ^ 3, ')'), sep =
                              '')
        )
        text(
          x = 3.5,
          y = .5,
          labels = paste('This watershed contributes ', pct, '% of spoil', sep = '')
        )
      })
      output$kWh <- renderText({
        ""
      })
    }
    else {
      output$barplot <- renderPlot({
        dat <-   (CountyCoal$SUM[CountyCoal$NAME == id()$id]) * 100
        barplot(
          height = dat / areas$val,
          horiz = T,
          log = "x",
          xlab = 'Burial depth (m)',
          las = 1,
          col = "darkgrey",
          xlim = c(0.1, 1000000),
          main = bquote(
            paste(
              'Depth of ',
              .(round(dat / (
                1000 * 1000 * 1000
              ), 3)),
              km ^ 3,
              " of Spoil, if Spread Over Various Areas"
            )
          )
        )
        text(((dat / areas$val) / 4) + 3,
             y = (1:nrow(areas)) * 1.2,
             labels = (areas$names),
             pos = 1,
             font = 2
        )
      })
      output$secondbar <- renderPlot({
        cols <- rep('gray', times = nrow(CountyCoal))
        cols[CountyCoal$NAME == id()$id] <- 'darkred'
        barplot(
          height = (CountyCoal$SUM * 100) / CountyCoal$coal_m3,
          names.arg = CountyCoal$NAME,
          main = "Spoil Deposited into Headwater Valleys per cubic meters of Coal",
          col = cols,
          ylab = "cubic metres",
          xlab = "County"
        )
      })
      txt <-
        CountyCoal$coal_kg[which(CountyCoal$NAME == as.character(id()$id))]
      output$kWh <- renderText({
        paste(
          txt / 8.141,
          "kilowatt hours of coal energy produced in",
          as.character(id()$id),
          "County."
        )
      })
    }
  })
  
  
  
  ##### Raster plots ----------------- Raster Plots ###########
  
  #retrieve rasters from file to plot in overhead slope views
  ff <- list.files("www/RastImg/OldE")
  fName <- reactive({
    ff[grep(id()$id, ff)]
  })
  
  
  cols <- c('grey1', 'darkred')
  
  #plot(c(0,100),c(0,100),col='white',axes=F,xlab='',ylab='')
  #text(x=50,y=50,labels='Maps not available for county data. Please
  #     select "Plot View" to see summary data.',cex=1.2)
  
  # Raster plots are all here ----------- #Rasters!!!!
  observe({
    output$rastplots <- renderImage({
      if (id()$group != 'Watersheds') {
        path <- normalizePath(file.path('www',
                                        'CountyWarning.png'))
        list(src = path)
      } else{
        #Call slope images if ES == 1 (Sloep explore)
        if (input$Diff == T) {
          path <- normalizePath(file.path('www',
                                          paste(
                                            "/RastImg/Diff/", fName(), sep = ''
                                          )))
          list(src = path,
               alt = fName())
        } else{
          if (input$ES == 1) {
            # If slope is called
            if (input$PrePost == T) {
              path <- normalizePath(file.path('www',
                                              paste(
                                                "/RastImg/OldS/", fName(), sep = ''
                                              )))
              list(src = path,
                   alt = fName())
            } else{
              # if New slope is called.
              path <- normalizePath(file.path('www',
                                              paste(
                                                "/RastImg/NewS/", fName(), sep = ''
                                              )))
              list(src = path,
                   alt = fName())
            }
          } else{
            # Calls elevation images.
            if (input$PrePost == T) {
              path <- normalizePath(file.path('www',
                                              paste(
                                                "/RastImg/OldE/", fName(), sep = ''
                                              )))
              list(src = path,
                   alt = fName())
            }
            else{
              path <- normalizePath(file.path('www',
                                              paste(
                                                "/RastImg/NewE/", fName(), sep = ''
                                              )))
              list(src = path,
                   alt = fName())
            }# Closes else path for New elevation map
          }#Closes else path for elevation plot
        }#Closes if diff plot is called
      }#Closes else if counties called
    }, deleteFile = F) #Closes render image call
    
    if (input$ES == 1) {
      #Slope density plots
      test <- eventReactive(input$MapMine_shape_click, {
        name1 <- id()$id
        if (id()$group == 'Watersheds') {
          name2 <- as.numeric(name1)
          return(slope[which(slope$HUC12 == name2), ])
        }
        else{
          return(slope[which(slope$NAME_1 == name1), ])
        }
      })
      
      # Generate first plot
      entireWatershed <- reactive({
        vals <- tapply(test()$value.deg, test()$variable, mean, na.rm = T)
        d <- round(vals[2] - vals[1], 1)
        s.dens <- ggplot(test(), aes(value.deg, fill = variable))
        s.dens.plot <-
          s.dens + geom_density(alpha = 0.8) + #ggtitle('c)')
          theme(legend.position = c(-.35, .7),
                legend.title = element_blank()) +
          scale_fill_manual(values = cols, name = '') + ylim(0, 0.07) +
          xlab('Slope (degrees)') + ylab('') + xlim(0, 50) +
          ggtitle(bquote(atop(
            paste(bold('Over Whole Watershed Area')),
            paste('slope changed by ', .(d), degree)
          ))) +
          scale_fill_manual(
            values = cols,
            breaks = c("OldSlope", "LidarSlope"),
            labels = c("Pre-Mining", "Post-Mining")
          )
        return (s.dens.plot)
      })
      
      
      #Generate second plot
      minedOnly <- reactive({
        dats <- test()[which(test()$mining == 'Mined'), ]
        vals <- tapply(dats$value.deg, dats$variable, mean, na.rm = T)
        d <- round(vals[2] - vals[1], 1)
        title <- bquote(atop(paste(bold(
          'Over Mined Areas'
        )),
        paste('slope changed by ', .(d), degree)))
        
        s.dens <-
          ggplot(test()[which(test()$mining == 'Mined'), ], aes(value.deg, fill =
                                                                  variable))
        s.dens.plot <-
          s.dens + geom_density(alpha = 0.8) + #ggtitle('c)')
          scale_fill_manual(values = cols, guide = FALSE) + xlim(0, 50) + ylim(0, 0.07) +
          xlab('Slope (degrees)') + ylab('Density') +
          ggtitle(title)
        scale_fill_manual(
          values = cols,
          breaks = c("OldSlope", "LidarSlope"),
          labels = c("Pre-Mining", "Post-Mining"),
          guide = F
        )
        return(s.dens.plot)
      })
      #Call plots
      output$gplots <- renderPlot({
        multiplot(minedOnly(), entireWatershed(), cols = 2)
      }, height = 400)
      
    } else{
      #Closes slope if statement
      
      #Ggplot to compare changes in Elevation---------####-------
      #Slope density plots
      test <- eventReactive(input$MapMine_shape_click, {
        name1 <- id()$id
        if (id()$group == 'Watersheds') {
          name2 <- as.numeric(name1)
          return(elev[which(elev$HUC12 == name2), ])
        }
        else{
          return(elev[which(elev$NAME_1 == name1), ])
        }
      })
      
      # Generate first plot
      # Get xlims
      xlims <- quantile(test()$value, c(0.01, .99))
      entireWatershed <- reactive({
        vals <- tapply(test()$value, test()$variable, mean, na.rm = T)
        d <- round(vals[2] - vals[1], 1)
        e.dens <- ggplot(test(), aes(value, fill = variable))
        e.dens.plot <-
          e.dens + geom_density(alpha = 0.8) + #ggtitle('c)')
          theme(legend.position = c(-.35, .8),
                legend.title = element_blank()) +
          xlim(xlims[1], xlims[2]) +
          scale_fill_manual(
            values = cols,
            name = '',
            breaks = c('OldElevFin', 'LidarFinal'),
            labels = c('Pre-Mining', 'Post-Mining')
          ) +
          xlab('Elevation (meters)') + ylab('')  +
          ggtitle(bquote(atop(
            paste(bold('Over Whole Watershed Area')),
            paste('Elevation changed by ', .(d), 'm')
          )))
        return (e.dens.plot)
      })
      
      #Generate second plot
      minedOnly <- reactive({
        dats <- test()[which(test()$mining == 'Mined'), ]
        vals <- tapply(dats$value, dats$variable, mean, na.rm = T)
        d <- round(vals[2] - vals[1], 1)
        e.dens <-
          ggplot(test()[which(test()$mining == 'Mined'), ], aes(value, fill = variable))
        e.dens.plot <-
          e.dens + geom_density(alpha = 0.8) + #ggtitle('c)')
          scale_fill_manual(values = cols, guide = FALSE)  +
          xlim(xlims[1], xlims[2]) +
          xlab('Elevation (meters)') + ylab('Density') +
          ggtitle(bquote(atop(
            paste(bold('Over Mined Areas')),
            paste('Elevation changed by ', .(d), 'm')
          )))
        return(e.dens.plot)
      })
      output$gplots <- renderPlot({
        multiplot(minedOnly(), entireWatershed(), cols = 2)
      }, height = 400)
    }
  })
})
