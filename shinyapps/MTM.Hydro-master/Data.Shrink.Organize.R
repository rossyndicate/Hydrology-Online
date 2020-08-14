#This is just a helper script to shrink and
#project shapefiles into .RData format and sparsify the timeseries data

#Script will not work as all data is stored locally, it is just here to keep a record of changes to data.

library(rgdal)
library(lubridate)
library(sp)
library(reshape2)
library(dygraphs)
library(xts)
source('~/Dropbox/Shared Science/NSF_MTM_All/MTM_MudRiver/src_functions/qc_functions_mr.R')
mytimezone <- 'Etc/GMT-5'

#Read in shapefiles for watershed outlines
setwd("~/Dropbox/Shared Science/NSF_MTM_All/MTM_TEMP")
all.sheds <- readOGR('watershedoutlines', 'ALL_merged')

#Convert to leaflet projection wgs84
all.sheds <- spTransform(all.sheds, CRS('+init=epsg:4326'))
all.sheds$Site <- c('RB', 'RB_1', 'REM', 'LF', 'MR14', 'MR2', 'LB', 'MR')

#Subset to our primary sites
isco.sheds <- all.sheds[all.sheds$Site %in% c('RB', 'LF', 'MR', 'LB'), ]
# #Check that it makes sense
# plot(isco.sheds, col = 1:4)
# legend(
#   'topleft',
#   pch = 15,
#   col = palette(),
#   legend = (isco.sheds$Site)
# )



#Read in Q water year data.
setwd('~/Dropbox/Shared Science/NSF_MTM_All/MTM_MudRiver/tidy_data/')
load('q.wat.year.2015.RData')
load('q.sc.yr.RData')




#Read in and merge baseflow data 
setwd(
  '~/Dropbox/Shared Science/NSF_MTM_All/MTM_MudRiver/tidy_data/Baseflow.For.Matt/H.and.H'
)
hh <- read.csv('MTM.Baseflow.H.and.H.csv', header = F)[, 1:5]

names(hh) <- c('min10', 'RB.hh', 'LF.hh', 'LB.hh', 'MR.hh')
#multiply baseflow by 6 to get mm/hr
for(i in 2:5){
  hh[,i] <- hh[,i]*6
}
hh$min10 <- qsct$min10




#Remove extraneous columns
q <- q[, 1:14]
q <- merge(q, hh, by = 'min10')

#RawData is at 10 minute interval, but I want to only display it at a 4 hour interval.
seq.4hr <- round_minute(rep(seq(q$min10[1]+60*60,q$min10[nrow(q)],length=365*6),each=4*6),60)
min.seq <- round_minute(seq(min(q$min10),max(q$min10),length=365*24*6),10)
min.df <- data.frame(min10=min.seq,hr=seq.4hr)

#Check data.
mx <- xts(cbind(Base=q$RB.hh,Full=q$RB.Q.mm),order.by=q$min10)
dygraph(mx) %>% dyOptions(useDataTimezone=T)




q <- na.omit(merge(q,min.df,by='min10',all.x=T))

#q$hr <- round_minute(q$min10, 60) # 60 minute rounding.


#Melt data by this new hour column
q.melt <-
  melt(q,
       id.vars = c('min10', 'hr'),
       measure.vars = names(q)[3:18])
q.hr <-
  dcast(
    q.melt,
    hr ~ variable,
    value.var = 'value',
    fun.aggregate = mean,
    na.rm = T
  )
q.hr$RB.Q.mm[q.hr$RB.Q.mm == 0] <- 10^-6

#Custom molten of q.hr to make shiny app easier
#Convert site to a factor
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

#Ok so now we have the data for a shiny app, just need to save it.
setwd("~/Dropbox/Shared Science/NSF_MTM_All/MTM_Shiny/MTM.Hydro")
save(q.hr, isco.sheds, file = 'Hydro.Shine.RData')
