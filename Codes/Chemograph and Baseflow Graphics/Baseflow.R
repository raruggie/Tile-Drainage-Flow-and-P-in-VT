# Code Mission:
# Plot a period of baseflow for a select set of events 
# The set of events chosen are listed in 'FFN' below
# This set was orginally chosen because it was the events i had
# discrete sample bottles and rainfall data for


# #clear memory
rm(list=ls(all=T))

# set wd
setwd("C:/RAR-Uvm/Data/Data for thesis")

# librarys
# library(tidyverse)
# library(grid)
# library(gridExtra)
# library(lubridate)

# read in nutrient and EC data, format dates
EC_TP<-read.csv(file = ".Baseflow Samples w EC.csv")
EC_TP$DateTime<-as.POSIXct(EC_TP$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')



# read in flow data
DCS<-read.csv(file = ".DCS BF 2021.csv")
DCN<-read.csv(file = ".DCN BF 2021.csv")
AHS<-read.csv(file = ".AHS BF 2021.csv")


# get bottle info: swap data frame name for each site and save csv of bottles DateTimes and Flow Rates to downloads folder
# write.csv(DCN[complete.cases(DCN$Bottle),],"/Users/ryanruggiero/Downloads/BF.csv", row.names = FALSE)

write.csv(f,"/Users/ryanruggiero/Downloads/event.csv", row.names = FALSE)

# remove bottle column
DCS$Bottle<-NULL
DCN$Bottle<-NULL
AHS$Bottle<-NULL

# remove NA's
DCS<-na.omit(DCS)
DCN<-na.omit(DCN)
AHS<-na.omit(AHS)

# format dates
DCS$DateTime<-as.POSIXct(DCS$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
DCN$DateTime<-as.POSIXct(DCN$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
AHS$DateTime<-as.POSIXct(AHS$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')

# Add site name
DCS$Site<-'DCS'
DCN$Site<-'DCN'
AHS$Site<-'AHS'

# break up EC, TP data by site
x<-EC_TP%>%nest(Sample:Flowrate)

# remove observations after 04/02/2021
DCS<-DCS[DCS$DateTime < as.POSIXct("2021-04-10", tz = 'UTC'),]
DCN<-DCN[DCN$DateTime < as.POSIXct("2021-04-10", tz = 'UTC'),]
AHS<-AHS[AHS$DateTime < as.POSIXct("2021-04-10", tz = 'UTC'),]

#### rainfall ####
rainfall<-read.csv(file= ".Tips_10042019-08022021.csv")
rainfall$DateTime<-as.POSIXct(rainfall$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
rainfall$Event<-0.01*2.54

hourly_precip <- aggregate(rainfall['Event'], list(hour=cut(as.POSIXct(rainfall$DateTime), "hour")), sum)
hourly_precip$hour<-as.POSIXct(hourly_precip$hour, format="%Y-%m-%d %H:%M:%S", tz = 'UTC')

#### set up ploting DF's with overlapping baseflow across all tiles  (AHS is limiter)

# DCS
d1<-dim(DCS)
DCS<-DCS[c(4371:d1[1]),]

# get nutrient data from x df
DCS_N<-x[[2]][[1]]
DCS_N<-DCS_N[which(DCS_N$DateTime >= DCS$DateTime[1]),]
my_factor_EC_S<-max(DCS$Flow)/max(DCS_N$EC)
my_factor_TP_S<-max(DCS$Flow)/max(DCS_N$TP)

# take mean over hour to smooth out noise
DCS$DateTime<-cut(DCS$DateTime, breaks = 'hour')
DCS<-aggregate(Flow~DateTime, DCS, mean)
DCS$DateTime<-as.POSIXct(DCS$DateTime, format="%Y-%m-%d %H:%M:%S", tz = 'UTC')

  
# DCN
d2<-dim(DCN)
DCN<-DCN[c(18605:d2[1]),]

# get nutrient data from x df
DCN_N<-x[[2]][[2]]
DCN_N<-DCN_N[which(DCN_N$DateTime >= DCN$DateTime[1]),]
my_factor_EC_N<-max(DCN$Flow)/max(DCN_N$EC)
my_factor_TP_N<-max(DCN$Flow)/max(DCN_N$TP)

# take mean over hour to smooth out noise
DCN$DateTime<-cut(DCN$DateTime, breaks = 'hour')
DCN<-aggregate(Flow~DateTime, DCN, mean)
DCN$DateTime<-as.POSIXct(DCN$DateTime, format="%Y-%m-%d %H:%M:%S", tz = 'UTC')

# AHS

# get nutrient data from x df
AHS_N<-x[[2]][[3]]
AHS_N<-AHS_N[which(AHS_N$DateTime >= AHS$DateTime[1]),]
my_factor_EC_A<-max(AHS$Flow)/max(AHS_N$EC)
my_factor_TP_A<-max(AHS$Flow)/max(AHS_N$TP)

# take mean over hour to smooth out noise
AHS$DateTime<-cut(AHS$DateTime, breaks = 'hour')
AHS<-aggregate(Flow~DateTime, AHS, mean)
AHS$DateTime<-as.POSIXct(AHS$DateTime, format="%Y-%m-%d %H:%M:%S", tz = 'UTC')

# rainfall
event_rain <- hourly_precip[hourly_precip$hour >= (AHS$DateTime[1]) & hourly_precip$hour < (AHS$DateTime[nrow(AHS)]),]
total_rain<-sum(event_rain$Event)

colnames(event_rain) <- c('DateTime', "Precip")


# plots

# need limts for plots
xmin <- min(AHS$DateTime,event_rain$DateTime)
xmax <- max(AHS$DateTime,event_rain$DateTime)


p1 <- ggplot() + geom_line(DCS, mapping = aes(x = DateTime, y = Flow)) +
  # geom_point(DCS_N, mapping = aes(x = DateTime, y = EC*my_factor_EC_S), color = 'Red') +
  geom_point(DCS_N, mapping = aes(x = DateTime, y = TP*my_factor_TP_S), color = 'Blue')+
  scale_y_continuous(name = "Flow (lps)\nDCS", sec.axis = sec_axis(trans = ~ . / my_factor_TP_S, name = "TP (ug/L)\n")) +
  xlim(xmin, xmax)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p1

p2 <- ggplot() + geom_line(DCN, mapping = aes(x = DateTime, y = Flow)) +
  # geom_point(DCN_N, mapping = aes(x = DateTime, y = EC*my_factor_EC_N), color = 'Red') +
  geom_point(DCN_N, mapping = aes(x = DateTime, y = TP*my_factor_TP_N), color = 'Blue')+
  scale_y_continuous(name = "Flow (lps)\nDCN", sec.axis = sec_axis(trans = ~ . / my_factor_TP_N, name = "TP (ug/L)\n")) +
  xlim(xmin, xmax)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p2

p3 <- ggplot() + geom_line(AHS, mapping = aes(x = DateTime, y = Flow)) +
  # geom_point(AHS_N, mapping = aes(x = DateTime, y = EC*my_factor_EC_A), color = 'Red') +
  geom_point(AHS_N, mapping = aes(x = DateTime, y = TP*my_factor_TP_A), color = 'Blue')+
  scale_y_continuous(name = "Flow (lps)\nAHS", sec.axis = sec_axis(trans = ~ . / my_factor_TP_A, name = "TP (ug/L)\n")) +
  xlim(xmin, xmax)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p3



p4 <- ggplot() + geom_col(event_rain, mapping = aes(x = DateTime, y = Precip), size = .5, color = "darkblue", fill = "white")+
  scale_y_continuous(name = "Rain (cm/hr)\n", sec.axis = sec_axis(trans = ~ . / 1, name = " \n")) +
  xlim(xmin, xmax)

p4

# assign plots to list
plist<-list(p1,p2,p3,p4)

# plot together
plot_collection <- grid.arrange(grobs = plist, ncol = 1, top = "TP in Baseflow 2021")

# # save worksapce dataframes for rmarkdown
# dfs<-Filter(function(x) is.list(get(x)) , ls())
# 
# dfs
# 
# dfs<-dfs[c(15)] # plot_collection
# 
# save(list=dfs, file="workspace5.RData")





