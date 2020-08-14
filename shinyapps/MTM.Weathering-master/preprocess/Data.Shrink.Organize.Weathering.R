#This is just a helper script to shrink and
#project shapefiles into .RData format and sparsify the timeseries data for quick display

#Script will not work as all data is stored locally until publication. 
# it is just here to keep a record of changes to data.

library(rgdal)
library(lubridate)
library(sp)
library(dygraphs)
library(xts)
library(ggplot2)
library(reshape2)
source('~/Dropbox/Shared Science/NSF_MTM_All/MTM_MudRiver/src_functions/qc_functions_mr.R')
mytimezone <- 'Etc/GMT-5'

#Read in shapefiles for watershed outlines
setwd("~/Dropbox/Shared Science/NSF_MTM_All/MTM_TEMP")
all.sheds <- readOGR('watershedoutlines', 'ALL_merged')
evnt <- read.csv('mtm_event_counter_consecutively.csv')



#Convert to leaflet projection wgs84
all.sheds <- spTransform(all.sheds, CRS('+init=epsg:4326'))
all.sheds$Site <- c('RB', 'RB_1', 'REM', 'LF', 'MR14', 'MR2', 'LB', 'MR')

#Subset to our primary sites
isco.sheds <- all.sheds[all.sheds$Site %in% c('RB', 'LF', 'MR', 'LB'), ]

#Add some site names and details. 
isco.sheds$Site <-
  factor(isco.sheds$Site, levels = c('RB', 'LF', 'LB', 'MR'))
isco.sheds$BigName <-
  factor(
    c(
      'RB (118 ha, ref.)',
      'LF (3,400 ha, ref.)',
      'LB (68 ha, 99% mined)',
      'MR (3,600 ha, 46% mined)'
    ),
    levels = c(
      'RB (118 ha, ref.)',
      'LF (3,400 ha, ref.)',
      'LB (68 ha, 99% mined)',
      'MR (3,600 ha, 46% mined)'
    )
  )


#Reorganize so leaflet plot allows for clicking on LB catchment
isco.sheds <- isco.sheds[c(1, 2, 4, 3), ]


# #Check that it makes sense
# plot(isco.sheds, col = 1:4)
# legend(
#   'topleft',
#   pch = 15,
#   col = palette(),
#   legend = (isco.sheds$Site)
# )


site.size <- data.frame(Site=c('LB','RB','LF','MR'),
                        size=c(68,118,3463,3672))

#Load flux data
load('~/Dropbox/Shared Science/NSF_MTM_All/MTM_MudRiver/tidy_data/Flux.Dat.RData')

#Remove NO3 data. 
flux.dat <- flux.dat[flux.dat$element != 'NO3.N',]
flux.dat$date <- as.Date(flux.dat$min10)

d.q <- dcast(flux.dat,date~site,value.var='Q.mm',mean) #mm/hr
d.sc <- dcast(flux.dat,date~site,value.var='sc',mean) #mm/hr
d.p <-  dcast(flux.dat,date~.,value.var='P',mean) 
names(d.p) <- c('date','Precip')


#For loop to sum flux over daily timesteps with min and max estimates for each site by element.
elements <- unique(flux.dat$element)
sites <- unique(flux.dat$site)

ct <- 0
f.l <- list()
c.l <- list()
fc.l <- list()
flux.l <- list()
f.t.l <- list()
for(i in 1:length(elements)){
  #Subset element
  el <- flux.dat[flux.dat$element == elements[i],]
  for(j in 1:length(sites)){
    ct <- ct+1
    #Subset by site
    el.site <- el[el$site == sites[j],]
    #Melt to get lwr,fit, and uppper in right spots
    fl.melt <- melt(el.site, id.vars=c('min10','date'),measure.vars=c('area.flx','min.area.flx','max.area.flx'))
    fl.melt$value <- fl.melt$value #(kg/ha/10min)
    fl.melt$value[fl.melt$value < 0] <- 0 # Floor flux to zero
    c.melt <- melt(el.site,id.vars=c('min10','date'),measure.vars=c('min.conc','conc','max.conc'))
    c.melt$value[c.melt$value < 0] <- 0 
    #Cast to put data in vectors for xts. 
    fl.cast <- dcast(fl.melt,date~variable,sum) #kg/ha/day
    c.cast <- dcast(c.melt,date~variable,mean)

    #remove first day
    fl.cast <- fl.cast[-1,]
    c.cast <- c.cast[-1,]
    fl.cast$Site <- sites[j]
    fl.cast$Ion <- elements[i]
    fl.cast$cume <- cumsum(fl.cast$area.flx)
    fl.cast$tot.fl <- fl.cast$area.flx*site.size[site.size$Site == sites[j],'size']
    fl.cast$p.cume <- fl.cast$cume/(max(fl.cast$cume))
    fl.cast$conc <- c.cast[,'conc']
    nm <- paste(elements[i],sites[j],sep='.')
    
    #Convert to xts format for quicker plotting and storage
    flx.xts <- xts(cbind('Lwr.flux'=fl.cast$min.area.flx,'Fit.flux'=fl.cast$area.flx,'Upr.flux'=fl.cast$max.area.flx),order.by=fl.cast$date)
    flx.tot.xts <- flx.xts*site.size[site.size$Site == sites[j],'size']
    c.xts <-  xts(cbind('Lwr.conc'=c.cast$min.conc,'Fit.conc'=c.cast$conc,'Upr.conc'=c.cast$max.conc),order.by=c.cast$date)
    
    names(flx.xts) <- paste(nm,c('Lwr.flux','Fit.flux','Upr.flux'),sep='.')
    names(flx.tot.xts) <- paste(nm,c('Lwr.flux','Fit.flux','Upr.flux'),sep='.')
    names(c.xts) <- paste(nm,c('Lwr.flux','Fit.flux','Upr.flux'),sep='.')
    
    
    f.l[[ct]] <- flx.xts
    c.l[[ct]] <- c.xts
    fc.l[[ct]] <- cbind(flx.xts,c.xts)
    f.t.l[[ct]] <- flx.tot.xts
    flux.l[[ct]] <- fl.cast
    names(f.l)[[ct]] <- paste(elements[i],sites[j],sep='.')
    names(c.l)[[ct]] <- paste(elements[i],sites[j],sep='.')
    names(fc.l)[[ct]] <- paste(elements[i],sites[j],sep='.')
    names(f.t.l)[[ct]] <- paste(elements[i],sites[j],sep='.')
  }
}

full.flux <- do.call(rbind,flux.l)



save(d.q,d.sc,d.p,f.l,c.l,fc.l,isco.sheds,full.flux,f.t.l,
      file="~/Dropbox/Shared Science/NSF_MTM_All/MTM_Shiny/MTM.Weathering/Flux.Shine.RData")






