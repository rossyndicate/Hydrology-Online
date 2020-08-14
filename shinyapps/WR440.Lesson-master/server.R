#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#Load libraries
library(shiny)
library(tidyverse)
library(xts)
library(dygraphs)
library(reshape2)
library(leaflet)
library(broom)
library(lubridate)
library(gridExtra)
library(grid)

#Load cleaned dataset, see "WQ.Storet.Data.Prep.Rmd" for details on where data comes from
#and how it was cleaned. 
load('data/ShinyFullDataset.RData')


#Set a ggplot theme for aesthetic purposes
matt_theme <- theme_set(theme_bw())
matt_theme<- theme_update(axis.line = element_line(colour = "black"),
                          panel.grid.major=element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          text=element_text(family='sans','plain','black',16,0.5,0.5,0,0),
                          plot.margin=unit(c(6,20,6,2),'pt')
)


# Define server logic to run application
shinyServer(function(input, output) {
  
#Generate leaflet (interactive map)
  output$map <- renderLeaflet({
    leaflet() %>%
      addPolygons(data=csub,color=csub$color,popup=paste((csub$HU_12_NAME)),group='Catchments') %>%
      addMarkers(data=station.sp,popup=paste('Station ID = ',station.sp$Station.ID)) %>%
      addCircles(data=min.mine,color='black',weight=7,
                 popup=paste(min.mine$CFS_DATE, min.mine$PH),group='Mine Outlets') %>%
      addProviderTiles('Esri.WorldTopoMap',group='Topo') %>%
      addProviderTiles('Esri.WorldImagery',group='Imagery') %>%
      addLayersControl(baseGroups=c('Topo','Imagery'),
                       overlayGroups=c('Catchments','Mine Outlets')
                       ) %>% hideGroup("Mine Outlets")
      
    })
  
#Generate interactive and zoomable discharge plot for first tab of app
  output$q <- renderDygraph({
    dygraph(q.xts,group='dy') %>%
      dyOptions(colors=c('red','blue'),strokeWidth=1.5) %>%
      dyAxis('y',label='Q (cms)')
  })
  
#Generate interactive and zoomable discharge plot for second tab of app
  output$q2 <- renderDygraph({
    dygraph(q.xts,group='dy') %>%
      dyOptions(colors=c('red','blue'),strokeWidth=1.5) %>%
      dyAxis('y',label='Q (cms)')
  })
  
#Generate the plot with analyte concentration data. 
  output$time.chem <- renderDygraph({
    d1 <- q.chem %>% 
      filter(variable == input$analyte) %>% 
      dcast(.,dateTime~Site,value.var='value',mean) 
   chem.dy <- xts(d1[,-1],order.by=d1$dateTime) %>%
     dygraph(.,group='dy')   %>%
     dyOptions(colors=c('red','blue'),strokeWidth=0, drawPoints=T,pointSize=4) %>%
     dyAxis('y',label='[Analyte] (mg/L)')
   if(!is.na(input$thresh)){
     chem.dy <- chem.dy %>% 
       dyLimit(limit=input$thresh,strokePattern='solid',color='green')
   }
   chem.dy
  })
  
  
#Generate boxplot and probability density function of data. Dynamic by window set in dygraph group
  #Add threshold line capacity
  output$diff <- renderPlot({    dts <- numeric()
  if(is.null(input$q_date_window)){
    dts <- c(min(q.chem$dateTime),max(q.chem$dateTime))
  }else{
    dts[1] <- (as.Date(input$q_date_window[[1]]))
    dts[2] <- (as.Date(input$q_date_window[[2]]))
  }
    q.diff <- q.chem %>%
      filter(!is.na(value)) %>%
      filter(variable==input$analyte) %>%
      filter(dateTime > dts[1] & dateTime < dts[2])
    
    box <- ggplot(q.diff,aes(x=Site,y=value,group=Site,fill=Site)) + 
      geom_boxplot(show.legend=F) +
      ylab('[Analyte] (mg/L)') +
      scale_fill_manual(name='',values=c('red','blue')) 
    
    if(!is.na(input$thresh)){
      box <- box + geom_hline(yintercept=input$thresh,col='green3',size=1.6)
    }
    
    dens <- ggplot(q.diff,aes(value,fill=Site)) + 
      geom_density(alpha=.7,show.legend = F) + 
      ylab('Density') +
      xlab('[Analyte] (mg/L)') + 
      scale_fill_manual(name='',values=c('red','blue'))
    
    if(!is.na(input$thresh)){
      dens <- dens + geom_vline(xintercept=input$thresh,col='green3',size=1.6)
    }
    
    
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(1,2,widths=c(0.5,0.5))))
    print(box, vp=viewport(layout.pos.row=1,layout.pos.col=1))
    print(dens, vp=viewport(layout.pos.row=1,layout.pos.col=2))
      
  })

#Generate plot of Q versus concentration based on dynamic window from dygraph
  #Give user a bunch of model options to play with. 
  output$chemostasis <- renderPlot({
    dts <- numeric()
    if(is.null(input$q2_date_window)){
      dts <- c(min(q.chem$dateTime),max(q.chem$dateTime))
    }else{
    dts[1] <- (as.Date(input$q2_date_window[[1]]))
    dts[2] <- (as.Date(input$q2_date_window[[2]]))
    }
    q.sub <- q.chem %>% 
      filter(dateTime > dts[1] & dateTime < dts[2]) %>%
      filter(!is.na(value)) %>%
      filter(variable == input$analyte1) %>%
      filter(value > 0) %>%
      mutate(month = month(dateTime))
    
    if(input$season=='all'){
      q.sub <- q.sub
    }
    if(input$season=='summer'){
      q.sub <- q.sub %>% filter(month %in% c(5,6,7,8,9,10))
    }
    
    if(input$season=='winter'){
      q.sub <- q.sub %>% filter(!month %in% c(5,6,7,8,9,10))
    }
    
      gplot <- ggplot(q.sub, aes(x=m3s,y=value,color=Site)) + 
      geom_point(shape=1,size=2) +
      geom_point(shape=1,size=2.1) + 
      scale_color_manual(name='',values=c('red','blue')) + 
      theme(legend.position=c(.8,.85)) + 
      xlab('Q (cms)') + 
      ylab('Analyte Concentration [mg/L]')
    if(input$model=='none'){
      print(gplot)
    }
    if(input$model=='yx'){
      mod <- lm(value ~ m3s*Site,data=q.sub) %>% glance(.)
      
      print(gplot + stat_smooth(method='lm') + 
              annotate("text",  x=Inf, y = Inf,
                           label =paste('p = ',round(mod$p.value,3),'R2 =',round(mod$adj.r.squared,2)), 
                           vjust=1.5, hjust=2.8,size=6))
    }
    if(input$model=='logx'){
      mod <- lm(value ~ log10(m3s)*Site,data=q.sub) %>% glance(.)
      
        print(gplot + scale_x_log10() + 
                annotate("text",  x=Inf, y = Inf,
                         label =paste('p = ',round(mod$p.value,3),'R2 =',round(mod$adj.r.squared,2)), 
                         vjust=1.5, hjust=2.8,size=6) + 
        stat_smooth(method='lm'))
    }
    if(input$model=='logy'){
      mod <- lm(log10(value) ~ m3s*Site,data=q.sub) %>% glance(.)
      
      print(gplot +  scale_y_log10() + 
              annotate("text",  x=Inf, y = Inf,
                       label =paste('p = ',round(mod$p.value,3),'R2 =',round(mod$adj.r.squared,2)), 
                       vjust=1.5, hjust=2.8,size=6) + 
              stat_smooth(method='lm'))
    }
    if(input$model=='logyx'){
      mod <- lm(log10(value) ~ log10(m3s)*Site,data=q.sub) %>% glance(.) 
      print(gplot + scale_y_log10() + 
        scale_x_log10() +
        annotate("text",  x=Inf, y = Inf,
                   label = paste('p = ',round(mod$p.value,3),'R2 =',round(mod$adj.r.squared,2)), 
                 vjust=1.5, hjust=2.8,size=6) + 
          stat_smooth(method='lm'))
    }
  })
})



