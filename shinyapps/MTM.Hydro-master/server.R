


#Load packages
#Load packages
library(shiny)
library(leaflet)
library(ggplot2)
library(dygraphs)
library(scales)
library(sp)
library(xts)
library(magicaxis)

#Load data
load('Hydro.Shine.RData')
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

dyBarChart <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "BarChart",
            path = system.file("plotters/barchart.js",
                               package = "dygraphs"))
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
g.d <- data.frame(Site=c('RB','LF','LB','MR'),OldSlope=c(19.3,17.5,20.5,21.6),
                  NewSlope=c(19.3,17.5,13.2,18.9),OldE=c(286,302,324,360),
                  NewE=c(286,302,337,362),VF=c(0,0,'10-14 million','162-185 million'),
                  FullName=c("Rich's Branch","Left Fork","Laurel Branch","Mud River"),
                  area=c('118','3,400','68','3,600'),stringsAsFactors=F)


#Setup a comparison table for plotting data on the same dygraph. 
c.col <- c("#008B00", "#66CD00", "#8B0000", "#CD4F39")


#Set line width for all plots
ld <- 4



#Reorganize so leaflet plot allows for clicking on LB catchment
#isco.sheds <- isco.sheds[c(1, 2, 4, 3), ]

#Setup shiny server
shinyServer(function(input, output) {
  #Setup Leaflet Map ------------------ #############
  
  output$MapMine <- renderLeaflet({
    #Setup color values
    shed.col <-
      colorFactor(c.col,
                  domain = isco.sheds$BigName)
    leaflet() %>% addProviderTiles("Esri.WorldTopoMap", group = 'Topo Map') %>%
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
                                         T)) %>% 
    setView(lng = -81.93603515625, lat = 38.1046650598288, zoom = 10)
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

  ######----------------------------Geomorph Tab -----------------------------------######

    #Generates a brief summary statement of physical catchment characteristics
  output$Sum.Text <- renderText({
    z <- g.d[g.d$Site == id()$id,]
    if(id()$id %in% c('LF','RB')){
      paste0(bquote(paste0(.(z$FullName),'(',.(z$Site),')','is a',.(z$area),
                         'ha reference catchment that has a mean slope of',.(z$OldSlope),'degrees',
                         'and a mean elevation of',.(z$OldE),
                         'm with shallow soils typically less than 2m deep.'))[-1])
    }else{
      paste0(bquote(paste0(.(z$FullName),'(',.(z$Site),')','is a',.(z$area),
                           'ha mined catchment that, before mining, had a mean slope of',.(z$OldSlope),'degrees',
                           'and a mean elevation of',.(z$OldE),
                           'm with shallow soils typically less than 2m deep.',
                           'After mining, the slope of',.(z$FullName),
                           'decreased by',.(z$OldSlope-z$NewSlope),'degrees',
                           'and elevation increased by',
                           .(z$NewE-z$OldE),'m. Additionally, we estimate that',
                           .(z$VF),'cubic meters of bedrock was exploded and deposited into headwater
                           valleys for this catchment'))[-1])
    }
  })
  
  #Grabs gif images to display catchments
  output$geogif <- renderImage({
    gname <- paste(id()$id,'.gif',sep='')
    path <- normalizePath(file.path('www',
                                    paste("gifs/",gname,sep='')))
    
    list(src=path,
         alt=gname)
  },deleteFile=F)


#Hydro.Flux ######---------------------------Hydro SC Flux Tab ----------------------------------######
  
  
  #Get comparison data for dygraph from radiobutton input
  c.dat <- reactive({
    if(input$comp != 3){
      n <- as.numeric(input$comp)
      c.site <- c.table[c.table$Site == id()$id,n+1] #Selects site and complimentary comparison data. 
    }else{c.site <- NULL}
    return(c.site)
  })
  
  #Setup basic dygraph output with Hyetograph on top.
  output$pplots <- renderDygraph({
    
    d.p$Precip[d.p$Precip == 0] <- NA
    
    s <- id()$id
    
    p.xts <- xts(d.p$Precip*24, order.by = d.p$date)
    
    names(p.xts) <- 'Precip'
    
    dygraph(p.xts, group = 'dy') %>%
      dyOptions(useDataTimezone = T,
        drawPoints = F,
        fillGraph = T,
        fillAlpha = 1,
        colors = 'blue',
        strokeWidth=3) %>%
      dyAxis('y', label = 'Precip (mm/day)', valueRange = c(80, 0)) %>%
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
    q.xts <- q.xts*24
    names(q.xts) <- q.col
    dygraph(q.xts, group = 'dy') %>%
      dyOptions(
        useDataTimezone = T,
        drawPoints = F,
        colors = dy.cols,
        strokeWidth = 2
      ) %>%
      dyAxis('y', label = 'Q (mm/day)')
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
##Reactive Baseplots ------------------Reactive Baseplots---------------###
  
  #plot that generates ecdf and cum.sum and cumulative diff plots based on DyGraph window. 
  output$cume.plot <- renderPlot({
    #Setup empty date vector and store dygraph dates in it, or, if null, use max window. 
    dts <- numeric()
    if(is.null(input$qplots_date_window)){
      dts <- c(min(q.hr$hr),max(q.hr$hr))
    }else{
      dts[1] <- as.POSIXct(as.Date(input$qplots_date_window[[1]]))
      dts[2] <- as.POSIXct(as.Date(input$qplots_date_window[[2]]))
    }
    #Grab data and subset color (inefficient but legible, could put in a single reactive expression)
    if(input$comp == 3){
      q.col <- paste(id()$id, '.Q.mm', sep = '')
      dy.cols <- c.table[c.table$Site==id()$id,'Color']
    }else{
      q.col <- paste(c(id()$id,c.dat()),'.Q.mm',sep='')
      dy.cols <- c(c.table[c.table$Site==id()$id,'Color'],
                   c.table[c.table$Site==c.dat(),'Color'])
    }
    #Plot if no comparison selected
    q.q <- q.hr[,c('hr',q.col)]
    q.sub <- q.q[q.q$hr > dts[1] & q.q$hr < dts[2],]
    q.cum1 <- 4*cumsum(q.sub[,q.col[1]])
    cd1 <- ecdf(q.sub[,q.col[1]])
    fd <- data.frame(y=4*knots(cd1),x=1-cd1(knots(cd1)))
    par(mfrow=c(1,3),mar=c(3,3,2,2),mgp=c(2,1,0),cex=1,font.lab=2)
    #Plot if comparison selected
    if(input$comp != 3){
      q.cum2 <- 4*cumsum(q.sub[,q.col[2]])
      cd2 <- ecdf(q.sub[,q.col[2]])
      fd2<- data.frame(y=4*knots(cd2),x=1-cd2(knots(cd2)))
      plot(y~x,data=fd,col=dy.cols[1],ylab='Q (mm/hr)',main='Flow Duration Curve',yaxt='n',
           xlab='Exceedance',type='l',lwd=ld,log='y',
           ylim=c(min(min(fd2$y,na.rm=T),min(fd$y,na.rm=T)),max(max(fd$y,na.rm=T),max(fd2$y,na.rm=T))))
      lines(y~x,data=fd2,col=dy.cols[2],lwd=ld)
      magaxis(2)
      plot(q.cum1~q.sub$hr,col=dy.cols[1],xlab='',
           ylab='Cumulative Q (mm)',type='l',main='Cumulative Q',
           lwd=ld,ylim=c(0,max(max(q.cum2),max(q.cum1))))
      lines(q.cum2~q.sub$hr,col=dy.cols[2],lwd=ld)
      if(id()$id %in% c('LF','RB')){
        diff <- q.cum2-q.cum1
        label <- paste0(c.dat(),'-',id()$id)
      }else{
      diff <- q.cum1-q.cum2
      label <- paste0(id()$id,'-',c.dat())
      }
      plot(diff~q.sub$hr,col='black',type='l',lwd=ld,
           ylab=paste('Q Diff (mm) (',label,')',sep=''),main='Q Diff',xlab='')
      abline(h=0,col='red')
    }else{
      plot(y~x,data=fd,col=dy.cols[1],ylab='Q (mm/hr)',xlab='Exceedance',type='l',lwd=ld,log='y',main='Flow Duration Curve',yaxt='n')
      magaxis(2)
      plot(q.cum1~q.sub$hr,col=dy.cols[1],xlab='',ylab='Cumulative Q (mm)',type='l',lwd=ld,main='Cumulative Q')
      plot(1,1,xaxt='n',yaxt='n',xlab='',ylab='')
      legend('center',lty=1,col=dy.cols[1],legend=id()$id)
    }
  })
  
  
  
#Baseflow Tab -----------------------------------Baseflow Tab ---------------------------------####  
  output$p.base <- renderDygraph({
    #Get an average P column
    p.xts <- xts(rowMeans(q.hr[,grep('.P',names(q.hr))]),order.by=q.hr$hr)
    p.xts[p.xts==0] <- NA
    dygraph(p.xts, group = 'dy') %>%
      dyOptions(useDataTimezone = T,
                drawPoints = F,
                fillGraph = T,
                fillAlpha = 1,
                colors = 'blue',
                strokeWidth=3) %>%
      dyAxis('y', label = 'Precip (mm/day)', valueRange = c(80, 0)) %>%
      dyBarChart()
    })
  
  
  output$q1.base <- renderDygraph({
    
    q1 <- xts(q.hr[,c('RB.hh','LB.hh')],order.by=q.hr$hr)
    
    
    names(q1) <- c('RB.baseflow','LB.baseflow')
    
    dygraph(q1,group='base',height='250px') %>%
    dySeries('RB.baseflow',color=c.col[1],strokeWidth=3) %>%
      dySeries('LB.baseflow',color=c.col[3],strokeWidth=3) %>%
      dyOptions(useDataTimezone=T) %>%
      dyAxis('y',label='Q (mm/hr)',valueRange=c(0,0.3))
  })
  
  output$q4.base <- renderDygraph({
    q4 <- xts(q.hr[,c('LF.hh','MR.hh')],order.by=q.hr$hr)
    
    names(q4) <- c('LF.baseflow','MR.baseflow')
    
    dygraph(q4,group='base',height='250px') %>%
      dySeries('LF.baseflow',color=c.col[2],strokeWidth=3) %>%
      dySeries('MR.baseflow',color=c.col[4],strokeWidth=3) %>%
      dyOptions(useDataTimezone=T) %>%
      dyAxis('y',label='Q (mm/hr)',valueRange=c(0,0.3))
  })



  
  output$b1 <- renderDygraph({
    #ran <- rng()
    q1 <- xts(q.hr[,c('RB.hh','RB.Q.mm')],order.by=q.hr$hr)
    names(q1) <- c('RB.baseflow','RB.Q')
    dygraph(q1,group='base',height='250px') %>%
      dySeries('RB.Q',color='darkblue',strokeWidth=3,fillGraph=F) %>%
      dySeries('RB.baseflow',color='cyan',strokeWidth=2.5,fillGraph=T) %>%
      dyOptions(useDataTimezone=T,fillAlpha=.8) %>%
      dyAxis('y',label='Q (mm/hr)')
  })
  
  output$b2 <- renderDygraph({
    #ran <- rng()
    q1 <- xts(q.hr[,c('LB.hh','LB.Q.mm')],order.by=q.hr$hr)
    names(q1) <- c('LB.baseflow','LB.Q')
    dygraph(q1,group='base',height='250px') %>%
      dySeries('LB.Q',color='darkred',strokeWidth=3,fillGraph=F) %>%
      dySeries('LB.baseflow',color='orange',strokeWidth=2.5,fillGraph=T) %>%
      dyOptions(useDataTimezone=T,fillAlpha=.8) %>%
      dyAxis('y',label='Q (mm/hr)')
  })
  
  output$b3 <- renderDygraph({
    #ran <- rng()
    q1 <- xts(q.hr[,c('LF.hh','LF.Q.mm')],order.by=q.hr$hr)
    names(q1) <- c('LF.baseflow','LF.Q')
    dygraph(q1,group='base',height='250px') %>%
      dySeries('LF.Q',color='darkblue',strokeWidth=3,fillGraph=F) %>%
      dySeries('LF.baseflow',color='cyan',strokeWidth=2.5,fillGraph=T) %>%
      dyOptions(useDataTimezone=T,fillAlpha=.8) %>%
      dyAxis('y',label='Q (mm/hr)')
  })
  
  output$b4 <- renderDygraph({
    q1 <- xts(q.hr[,c('MR.hh','MR.Q.mm')],order.by=q.hr$hr)
    names(q1) <- c('MR.baseflow','MR.Q')
    dygraph(q1,group='base',height='250px') %>%
      dySeries('MR.Q',color='darkred',strokeWidth=3,fillGraph=F) %>%
      dySeries('MR.baseflow',color='orange',strokeWidth=2.5,fillGraph=T) %>%
      dyOptions(useDataTimezone=T,fillAlpha=.8) %>%
      dyAxis('y',label='Q (mm/hr)')
  })
  
})
