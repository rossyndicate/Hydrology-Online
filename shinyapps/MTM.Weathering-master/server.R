# MTM WEATHERING FLUX SHINE
# Server side of app to generate displays.

# Add in equation for how flux is calculated. Add in R2.

#Load packages
library(shiny)
library(leaflet)
library(ggplot2)
library(dygraphs)
library(scales)
library(sp)
library(xts)
library(magicaxis)

dyBarChart <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "BarChart",
            path = system.file("plotters/barchart.js",
                               package = "dygraphs"))
}

#Load data
load('Flux.Shine.RData')
#Loads 7 data types.
#d.p is daily mean precip in mm/hr
#d.q is daily mean Q in mm/hr
#d.sc is daily mean SC in us/cm
#c.l is a list of daily mean concentration
d.q <- d.q[-1, ]

#Order flux dat sites.

full.flux$Site <- factor(full.flux$Site, c('RB', 'LF', 'LB', 'MR'))
full.flux$Site.Ion <- paste(full.flux$Ion, full.flux$Site, sep = '.')

#Setup larger font size.
theme_set(theme_grey(base_size = 16))

#Setup a comparison table for plotting data on the same dygraph.
c.col <- c("#008B00", "#66CD00", "#8B0000", "#CD4F39")

relcum <- function(x) {
  y = cumsum(x) / sum(x)
  return(y)
}



#Create a comparison data table.
c.table <- data.frame(
  Site = c('RB', 'LF', 'LB', 'MR'),
  Treatment = c('LB', 'MR', 'RB', 'LF'),
  Size = c('LF', 'RB', 'MR', 'LB'),
  Color = c.col,
  stringsAsFactors = F
)


#Setup a color lookup table for elements.
ions <- c('Ca', 'Na', 'K', 'Mg', 'SO4', 'HCO3', 'Cl', 'TDS')
i.col <- data.frame(
  ions = ions,
  col = colorRampPalette(c('#313695', '#fdae61', '#a50026'))(8),
  mcol = colorRampPalette(c('#053061', '#fddbc7', '#b2182b'))(8),
  stringsAsFactors = F
)




#Create a data frame with display text for geomorph section. Data from DEMs in Ross 2016
g.d <-
  data.frame(
    Site = c('RB', 'LF', 'LB', 'MR'),
    OldSlope = c(19.3, 17.5, 20.5, 21.6),
    NewSlope = c(19.3, 17.5, 13.2, 18.9),
    OldE = c(286, 302, 324, 360),
    NewE = c(286, 302, 337, 362),
    VF = c(0, 0, '10-14 million', '162-185 million'),
    FullName = c("Rich's Branch", "Left Fork", "Laurel Branch", "Mud River"),
    area = c('118', '3,400', '68', '3,600'),
    stringsAsFactors = F
  )


#Set line width for all plots
ld <- 3



#Setup shiny server
shinyServer(function(input, output) {
  ############## ------------------ Leaflet Map ---------------- ################
  
  output$MapMine <- renderLeaflet({
    #Setup color values
    shed.col <-
      colorFactor(c.col,
                  domain = isco.sheds$BigName)
    leaflet() %>% addProviderTiles("Esri.WorldTopoMap", group = 'Topo Mahttps://www.dropbox.com/s/kjkhwip0t8erh3a/MTM_PQ_data.csv?dl=0p') %>%
      addProviderTiles('Esri.WorldImagery', group = 'Aerial Imagery') %>%
      addPolygons(
        data = isco.sheds,
        weight = 3,
        smooth = 0,
        stroke = T,
        fillOpacity = 0.2,
        color = shed.col(isco.sheds$BigName),
        popup = paste('Site = ', isco.sheds$Site, sep = ''),
        layerId = isco.sheds$Site,
        group = 'Catchments'
      ) %>%
      # addLegend(
      #   position = 'topright',
      #   values = isco.sheds$BigName,
      #   labels = isco.sheds$BigName,
      #   pal = shed.col,
      #   title = 'Study Catchment'
      # ) %>%
      addLayersControl(
        position = 'topright',
        baseGroups = c('Topo Map', 'Aerial Imagery'),
        overlayGroups = c('Catchments'),
        options = layersControlOptions(collapsed = F, autoZIndex =
                                         T)
      ) %>%
      setView(lng = -81.93603515625,
              lat = 38.1046650598288,
              zoom = 10)
  })
  
  
  #Get id from map click
  id <- reactive({
    validate(
      need(
        input$MapMine_shape_click != "",
        "Please select a catchment from the map to the left to view plots and data.
        App may take a few seconds to load data after selecting data (depending on internet connection speed)."
      )
      )
    (input$MapMine_shape_click)
  })
  
#Geomorph.Tab  ######----------------------------Geomorph Tab -----------------------------------######
  
  #Generates a brief summary statement of physical catchment characteristics
  output$Sum.Text <- renderText({
    z <- g.d[g.d$Site == id()$id, ]
    if (id()$id %in% c('LF', 'RB')) {
      paste0(bquote(
        paste0(
          .(z$FullName),
          '(',
          .(z$Site),
          ')',
          'is a',
          .(z$area),
          'ha reference catchment that has a mean slope of',
          .(z$OldSlope),
          'degrees',
          'and a mean elevation of',
          .(z$OldE),
          'm with shallow soils typically less than 2m deep.'
        )
      )[-1])
    } else{
      paste0(bquote(
        paste0(
          .(z$FullName),
          '(',
          .(z$Site),
          ')',
          'is a',
          .(z$area),
          'ha mined catchment that, before mining, had a mean slope of',
          .(z$OldSlope),
          'degrees',
          'and a mean elevation of',
          .(z$OldE),
          'm with shallow soils typically less than 2m deep.',
          'After mining, the slope of',
          .(z$FullName),
          'decreased by',
          .(z$OldSlope - z$NewSlope),
          'degrees',
          'and elevation increased by',
          .(z$NewE - z$OldE),
          'm. Additionally, we estimate that',
          .(z$VF),
          'cubic meters of bedrock was exploded and deposited into headwater
          valleys for this catchment'
        )
        )[-1])
    }
    })
  
  #Grabs gif images to display catchments
  output$geogif <- renderImage({
    gname <- paste(id()$id, '.gif', sep = '')
    path <- normalizePath(file.path('www',
                                    paste("gifs/", gname, sep = '')))
    
    list(src = path,
         alt = gname)
  }, deleteFile = F)
  
  
#Hydro.SC.Tab ######---------------------------Hydrology/Conductivity Tab ----------------------------------######
  
  
  
  #Get comparison data for dygraph from radiobutton input
  c.dat <- reactive({
    if (input$comp != 3) {
      n <- as.numeric(input$comp)
      c.site <-
        c.table[c.table$Site == id()$id, n + 1] #Selects site and complimentary comparison data.
    } else{
      c.site <- NULL
    }
    return(c.site)
  })
  
  #Setup basic dygraph output with Hyetograph on top.
  output$pplots <- renderDygraph({
    p.col <- paste(id()$id, '.P', sep = '')
    d.p$Precip[d.p$Precip == 0] <- NA
    
    p.xts <- xts(d.p$Precip, order.by = d.p$date)
    dygraph(p.xts, group = 'dy') %>%
      dyOptions(
        useDataTimezone = T,
        drawPoints = F,
        fillGraph = T,
        fillAlpha = 1,
        colors = 'blue',
        strokeWidth = ld
      ) %>%
      dyAxis('y', label = 'Daily Mean P (mm/hr)', valueRange = c(5, 0)) %>%
      dyBarChart()
  })
  
  #Discharge plot with option to compare catchment to other sites.
  output$qplots <- renderDygraph({
    #Get data and display color information.
    if (input$comp == 3) {
      q.col <- id()$id
      dy.cols <- c.table[c.table$Site == id()$id, 'Color']
    } else{
      q.col <- c(id()$id, c.dat())
      dy.cols <- c(c.table[c.table$Site == id()$id, 'Color'],
                   c.table[c.table$Site == c.dat(), 'Color'])
    }
    if (id()$id %in% c('MR', 'LB')) {
      q.col <- rev(q.col)
      dy.cols <- rev(dy.cols)
    }
    q.q <- d.q[, q.col]
    q.xts <- xts(q.q, order.by = d.q$date)
    names(q.xts) <- q.col
    dygraph(q.xts, group = 'dy') %>%
      dyOptions(
        useDataTimezone = T,
        drawPoints = F,
        colors = dy.cols,
        strokeWidth = 2
      ) %>%
      dyAxis('y', label = 'Q (mm/hr)')
  })
  
  output$scplots <- renderDygraph({
    #Get data and display color information.
    if (input$comp == 3) {
      q.col <- id()$id
      dy.cols <- c.table[c.table$Site == id()$id, 'Color']
    } else{
      q.col <- c(id()$id, c.dat())
      dy.cols <- c(c.table[c.table$Site == id()$id, 'Color'],
                   c.table[c.table$Site == c.dat(), 'Color'])
    }
    if (id()$id %in% c('MR', 'LB')) {
      q.col <- rev(q.col)
      dy.cols <- rev(dy.cols)
    }
    q.q <- d.sc[, q.col]
    sc.xts <- xts(q.q, order.by = d.sc$date)
    names(sc.xts) <- q.col
    dygraph(sc.xts, group = 'dy') %>%
      dyOptions(
        useDataTimezone = T,
        drawPoints = F,
        colors = dy.cols,
        strokeWidth = 2
      ) %>%
      dyAxis('y', label = 'SC (uS/cm)')
  })
  
  
  ######---------------------------Elemental Flux Tab -----------------------------------------#######
  
  
#Daily.Flux  ####-----------------------------Dygraph portion --------------------------------------------######
  output$w1.q <- renderDygraph({
    #Get Q columns
    q.xts <- xts(d.q[, c('RB', 'LB')], order.by = d.q$date)
    names(q.xts) <- c('RB.Q', 'LB.Q')
    dygraph(q.xts, group = 'base') %>%
      dyOptions(
        drawPoints = F,
        colors = c(c.col[1], c.col[3]),
        strokeWidth = ld
      ) %>%
      dyAxis('y', 'Q (mm/hr)')
  })
  
  output$w4.q <- renderDygraph({
    #Get Q columns
    q.xts <- xts(d.q[, c('LF', 'MR')], order.by = d.q$date)
    names(q.xts) <- c('LF.Q', 'MR.Q')
    dygraph(q.xts, group = 'base') %>%
      dyOptions(
        useDataTimezone = T,
        drawPoints = F,
        colors = c(c.col[2], c.col[4]),
        strokeWidth = ld
      ) %>%  dyAxis('y', 'Q (mm/hr)')
  })
  
  #Reactive expression to get data depending on user input (areal flux vs flux vs conc, etc...)
  flx.dat <- reactive({
    clmn <-
      paste(input$Ion, rep(c('RB', 'LB'), each = length(input$Ion)), sep = '.')
    indx <- which(names(f.l) %in% clmn)
    
    clmn1 <-
      paste(input$Ion, rep(c('LF', 'MR'), each = length(input$Ion)), sep = '.')
    indx1 <- which(names(f.l) %in% clmn1)
    
    if (input$flx == 1) {
      q1 <- do.call(cbind, f.l[indx])
      q2 <- do.call(cbind, f.l[indx1])
      label <- 'Daily Flux (kg/ha/day)'
    }
    if (input$flx == 2) {
      q1 <- do.call(cbind, f.t.l[indx])
      q2 <- do.call(cbind, f.t.l[indx1])
      label <- 'Daily Flux (kg/day)'
    }
    if (input$flx == 3) {
      q1 <- do.call(cbind, c.l[indx])
      q2 <- do.call(cbind, c.l[indx1])
      label <- 'Mean Daily Conc (mg/l) \n'
    }
    dy.dat <- list(q1, q2, label)
    return(dy.dat)
  })
  
  output$w1.flux <- renderDygraph({
    clmn <-
      paste(input$Ion, rep(c('RB', 'LB'), each = length(input$Ion)), sep = '.')
    indx <- which(names(f.l) %in% clmn)
    q1 <- flx.dat()[[1]]
    lbl <- flx.dat()[[3]]
    g <- dygraph(q1, group = 'base') %>%
      dyAxis('y', lbl) %>%
      dyOptions(drawPoints = F)
    for (i in 1:length(indx)) {
      sbs <- (1:3 + (i - 1) * 3)
      series <- dimnames(q1[, sbs])[[2]]
      nm <- sub('.Lwr.flux', '', series[1])
      ion <- unlist(strsplit(nm, '.', fixed = T))[1]
      if (grepl('RB', series[1]) == T) {
        g <- dySeries(
          g,
          name = series,
          label = nm,
          color = i.col[i.col$ions == ion, 'col'],
          strokeWidth = ld,
          strokePattern = 'dashed'
        )
      }
      else{
        g <- dySeries(
          g,
          name = series,
          label = nm,
          color = i.col[i.col$ions == ion, 'mcol'],
          strokeWidth = ld
        )
      }
    }
    g
  })
  
  output$w4.flux <- renderDygraph({
    clmn <-
      paste(input$Ion, rep(c('RB', 'LB'), each = length(input$Ion)), sep = '.')
    indx <- which(names(f.l) %in% clmn)
    q1 <- flx.dat()[[2]]
    lbl <- flx.dat()[[3]]
    g <- dygraph(q1, group = 'base') %>%
      dyAxis('y', lbl) %>%
      dyOptions(drawPoints = F)
    for (i in 1:length(indx)) {
      sbs <- (1:3 + (i - 1) * 3)
      series <- dimnames(q1[, sbs])[[2]]
      nm <- sub('.Lwr.flux', '', series[1])
      ion <- unlist(strsplit(nm, '.', fixed = T))[1]
      if (grepl('LF', series[1]) == T) {
        g <- dySeries(
          g,
          name = series,
          label = nm,
          color = i.col[i.col$ions == ion, 'col'],
          strokeWidth = 3,
          strokePattern = 'dashed'
        )
      }
      else{
        g <- dySeries(
          g,
          name = series,
          label = nm,
          color = i.col[i.col$ions == ion, 'mcol'],
          strokeWidth = 3
        )
      }
    }
    g
  })
  
  
#Cume.Flux ###########--------------------------Reactive Cumulative Flux --------------------------###########
  
  
  
  output$Flux <- renderPlot({
    
      dat <- full.flux[full.flux$Ion %in% input$Ion, ]
      dts <- c(min(full.flux$date), max(full.flux$date))
      if (!is.null(input$w1.q_date_window)) {
        dts[1] <- strftime(input$w1.q_date_window[[1]])
        dts[2] <-  strftime(input$w1.q_date_window[[2]])
      }
      dat <- dat[dat$date > dts[1] & dat$date < dts[2], ]
      dat <- dat[order(dat$Site.Ion, dat$date), ]
      if(input$flx != 3){
        if(input$flx != 2){
          cume.list <- tapply(dat$area.flx, dat$Site.Ion, cumsum)
          dat$cume1 <- unlist(cume.list)
        }else{
          cume.list <- tapply(dat$tot.fl, dat$Site.Ion, cumsum)
          dat$cume1 <- unlist(cume.list)
        }
        p.max <- function(x){
          y=x/(max(x))
          return(y)
        }
        if (input$gopt == 3) {
          dat$cume1 <- dat$p.cume
        }
        brks <- c(min(dat$date), mean(dat$date), max(dat$date))
        g1 <- ggplot(dat, aes(x = date, y = cume1, color = Site)) +
          facet_wrap( ~ Ion, ncol = 2, scales = 'free_y') +
          geom_point() + ylab('Cumulative Flux (kg/ha)') +
          theme(legend.position = 'bottom') +
          scale_x_date(labels = date_format("%m/%y"), breaks = brks) +
          scale_colour_manual(values = c.col)
        if(input$gopt == 2) {
          g1 + scale_y_log10(limits=c(0.1,NA))
        }else{g1}
    }else{
      g2 <- ggplot(dat,aes(x=Site,y=conc,fill=Site)) +
        facet_wrap(~Ion, ncol=2,scales='free_y') +
        geom_boxplot() + scale_fill_manual(values=c.col,name='') 
      g2 + ylab('Modeled Concentration (mg/l)') + 
        theme(legend.position = 'bottom') 
    }
    
  })
  
  
  
  
#Stack.FLux  ######---------------------Display if Stacked Flux is Chosen --------#####

stack.ions <- c('Na','K','Ca','Mg','Cl','HCO3','SO4')
stack.cols <- colorRampPalette(c('#313695', '#DF744D', '#a50026'))(7)
stack.cols
  output$rb.flx <- renderDygraph({
    
    clmn <- paste(stack.ions, rep(c('RB'), each = length(stack.ions)), sep = '.')
    indx <- which(names(f.l) %in% clmn)
    f <- do.call(cbind,f.l[indx])
    f <- f[,grep('Fit.flux',names(f))]
    names(f) <- sub('.Fit.flux','',names(f))
    #Reorder
    f <- f[,rev(c(6,7,3,4,5,2,1))]
    dygraph(f) %>% dyOptions(stackedGraph=T,colors=stack.cols,
                             fillAlpha=.8) %>% 
      dyHighlight(highlightSeriesBackgroundAlpha = .3,
                  highlightSeriesOpts=list(strokeWidth=3))
  
    })
  
  output$lb.flx <- renderDygraph({
    clmn <- paste(stack.ions, rep(c('LB'), each = length(stack.ions)), sep = '.')
    indx <- which(names(f.l) %in% clmn)
    f <- do.call(cbind,f.l[indx])
    f <- f[,grep('Fit.flux',names(f))]
    names(f) <- sub('.Fit.flux','',names(f))
    #Reorder
    f <- f[,rev(c(6,7,3,4,5,2,1))]
    dygraph(f) %>% dyOptions(stackedGraph=T,colors=stack.cols,
                             fillAlpha=.8) %>% 
      dyHighlight(highlightSeriesBackgroundAlpha = .3,
                  highlightSeriesOpts=list(strokeWidth=3))
  })
  
  output$lf.flx <- renderDygraph({
    clmn <- paste(stack.ions, rep(c('LF'), each = length(stack.ions)), sep = '.')
    indx <- which(names(f.l) %in% clmn)
    f <- do.call(cbind,f.l[indx])
    f <- f[,grep('Fit.flux',names(f))]
    names(f) <- sub('.Fit.flux','',names(f))
    #Reorder
    f <- f[,rev(c(6,7,3,4,5,2,1))]
    dygraph(f) %>% dyOptions(stackedGraph=T,colors=stack.cols,
                             fillAlpha=.8) %>% 
      dyHighlight(highlightSeriesBackgroundAlpha = .3,
                  highlightSeriesOpts=list(strokeWidth=3))
  })
  
  output$mr.flx <- renderDygraph({
    clmn <- paste(stack.ions, rep(c('MR'), each = length(stack.ions)), sep = '.')
    indx <- which(names(f.l) %in% clmn)
    f <- do.call(cbind,f.l[indx])
    f <- f[,grep('Fit.flux',names(f))]
    names(f) <- sub('.Fit.flux','',names(f))
    #Reorder
    f <- f[,rev(c(6,7,3,4,5,2,1))]
    dygraph(f) %>% dyOptions(stackedGraph=T,colors=stack.cols,
                             fillAlpha=.8) %>% 
      dyHighlight(highlightSeriesBackgroundAlpha = .3,
                  highlightSeriesOpts=list(strokeWidth=3))
  })
  
  })

