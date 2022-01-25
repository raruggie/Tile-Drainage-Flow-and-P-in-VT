# Code Mission:
# Plot chemographs for a select set of events 
# The set of events chosen are listed in 'FFN' below
# This set was orginally chosen because it was the events i had
# discrete sample bottles and rainfall data for


# #clear memory
rm(list=ls(all=T))

# set working directory
setwd("C:/RAR-Uvm/Data/Data for thesis")


#### librarys ####
# library(pracma)
# library(lubridate)
# library(grid)
# library(tidyverse)
# library(zoo)

##### Flow file names ####
FFN<-c('AHSSUB - Flow - 03262020.csv',
       'AHSSUB - Flow - 03292020.csv',
       'AHSSUB - Flow - 04132020.csv',
       'AHSSUB - Flow - 04272020.csv',
       'DCSSUB - Flow - 05122020.csv',
       'DCNSUB - Flow - 05122020.csv',
       'DCSSUB - Flow - 08042020.csv',
       'DCNSUB - Flow - 08042020.csv',
       'DCSSUB - Flow - 09302020.csv',
       'DCNSUB - Flow - 09302020.csv',
       'DCNSUB - Flow - 12012020.csv',
       'AHSSUB - Flow - 12012020.csv',
       'DCNSUB - Flow - 12252020.csv',
       'AHSSUB - Flow - 12252020.csv',
       'DCNSUB - Flow - 01172021.csv',
       'AHSSUB - Flow - 01172021.csv',
       'DCSSUB - Flow - 03112021.csv',
       'DCNSUB - Flow - 03112021.csv',
       'DCSSUB - Flow - 04162021.csv',
       'DCNSUB - Flow - 04162021.csv',
       'DCSSUB - Flow - 04222021.csv',
       'DCNSUB - Flow - 04222021.csv',
       'DCSSUB - Flow - 04302021.csv',
       'DCNSUB - Flow - 04302021.csv',
       'DCSSUB - Flow - 05062021.csv',
       'DCNSUB - Flow - 05062021.csv',
       'DCSSUB - Flow - 07022021.csv',
       'DCNSUB - Flow - 07022021.csv',
       'DCSSUB - Flow - 07032021.csv',
       'DCNSUB - Flow - 07032021.csv',
       'DCSSUB - Flow - 07092021.csv',
       'DCNSUB - Flow - 07092021.csv',
       'DCSSUB - Flow - 07132021.csv',
       'DCNSUB - Flow - 07132021.csv',
       'DCSSUB - Flow - 07142021.csv',
       'DCNSUB - Flow - 07142021.csv',
       'DCSSUB - Flow - 07182021.csv',
       'DCNSUB - Flow - 07182021.csv',
       'DCSSUB - Flow - 07192021.csv',
       'DCNSUB - Flow - 07192021.csv',
       'DCSSUB - Flow - 07212021.csv',
       'DCNSUB - Flow - 07212021.csv'
)

#### EC,TP File names ####
ETN<-c('AHSSUB - EC, TP - 03262020.csv',
       'AHSSUB - EC, TP - 03292020.csv',
       'AHSSUB - EC, TP - 04132020.csv',
       'AHSSUB - EC, TP - 04272020.csv',
       'DCSSUB - EC, TP - 05122020.csv',
       'DCNSUB - EC, TP - 05122020.csv',
       'DCSSUB - EC, TP - 08042020.csv',
       'DCNSUB - EC, TP - 08042020.csv',
       'DCSSUB - EC, TP - 09302020.csv',
       'DCNSUB - EC, TP - 09302020.csv',
       'DCNSUB - EC, TP - 12012020.csv',
       'AHSSUB - EC, TP - 12012020.csv',
       'DCNSUB - EC, TP - 12252020.csv',
       'AHSSUB - EC, TP - 12252020.csv',
       'DCNSUB - EC, TP - 01172021.csv',
       'AHSSUB - EC, TP - 01172021.csv',
       'DCSSUB - EC, TP - 03112021.csv',
       'DCNSUB - EC, TP - 03112021.csv',
       'DCSSUB - EC, TP - 04162021.csv',
       'DCNSUB - EC, TP - 04162021.csv',      
       'DCSSUB - EC, TP - 04222021.csv',
       'DCNSUB - EC, TP - 04222021.csv',
       'DCSSUB - EC, TP - 04302021.csv',
       'DCNSUB - EC, TP - 04302021.csv',
       'DCSSUB - EC, TP - 05062021.csv',
       'DCNSUB - EC, TP - 05062021.csv',
       'DCSSUB - EC, TP - 07022021.csv',
       'DCNSUB - EC, TP - 07022021.csv',
       'DCSSUB - EC, TP - 07032021.csv',
       'DCNSUB - EC, TP - 07032021.csv',
       'DCSSUB - EC, TP - 07092021.csv',
       'DCNSUB - EC, TP - 07092021.csv',
       'DCSSUB - EC, TP - 07132021.csv',
       'DCNSUB - EC, TP - 07132021.csv',
       'DCSSUB - EC, TP - 07142021.csv',
       'DCNSUB - EC, TP - 07142021.csv',
       'DCSSUB - EC, TP - 07182021.csv',
       'DCNSUB - EC, TP - 07182021.csv',
       'DCSSUB - EC, TP - 07192021.csv',
       'DCNSUB - EC, TP - 07192021.csv',
       'DCSSUB - EC, TP - 07212021.csv',
       'DCNSUB - EC, TP - 07212021.csv'
)


#### number of site events ####
NoE<-as.numeric(length(FFN))

#### rainfall ####
rainfall<-read.csv(file= ".Tips_10042019-08022021.csv")
rainfall$DateTime<-as.POSIXct(rainfall$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
rainfall$Event<-0.01*2.54

hourly_precip <- aggregate(rainfall['Event'], list(hour=cut(as.POSIXct(rainfall$DateTime), "30 min")), sum)
hourly_precip$hour<-as.POSIXct(hourly_precip$hour, format="%Y-%m-%d %H:%M:%S", tz = 'UTC')

# hourly_precip<-hourly_precip[hourly_precip$Event > 0.0254*2,]

##### loop site events ####

i<-37


for (i in seq(1,NoE)){
  gc()
  
  #### read in flow data, format dataframes, get event and site names saved ####

  # read in flow data, format dates
  hydrograph<-read.csv(file= FFN[i])
  
  # remove anhy additional columns (like 'file')
  hydrograph<-hydrograph[,-3]
  
  # need to account for two digit dates (files prior to 05062021)
  # to do this, first run two digit, is result is NA, then tyou know it is a 4 digit 
  test_D <- as.POSIXlt(hydrograph$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
  
  if (is.na(test_D[1])) {
    hydrograph$DateTime <- as.POSIXlt(hydrograph$DateTime, format="%m/%d/%Y %H:%M", tz = 'UTC')
  } else {
    hydrograph$DateTime <- as.POSIXlt(hydrograph$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
  }
  
  hydrograph$Time<-(as.numeric(hydrograph$DateTime)-as.numeric(hydrograph$DateTime[1]))/(60*60)
  hydrograph<-na.omit(hydrograph)
  d<-dim(hydrograph)
  
  ## read in EC/TP + SRP data
  EC_TP<-read.csv(file= ETN[i])
  
  # need to make uniform column names: was getting 'ï..' appeneded to first column header, no idea
  colnames(EC_TP)<-c("DateTime", "EC..mS.", "TP", "SRP", "Sample.Number")
  
  # again need to account for 2 and 4 digit year combos in input files

  test_D <- as.POSIXct(EC_TP$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
  
  if (is.na(test_D[1])) {
    EC_TP$DateTime <- as.POSIXlt(EC_TP$DateTime, format="%m/%d/%Y %H:%M", tz = 'UTC')
  } else {
    EC_TP$DateTime <- as.POSIXlt(EC_TP$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
  }
  
  EC_TP$plotting<-max(hydrograph$Flow)/2
  
  # remove bottles that occur after event end (events were orginally extended to include all bottles, but after looking at them some of the tails were too long, so csv files were trunkated)
  EC_TP<-EC_TP[which(EC_TP$DateTime < hydrograph$DateTime[d[1]]),]
  
  # merge
  loadograph<- merge(x = hydrograph,y = EC_TP, by.x = "DateTime", by.y = "DateTime", all = TRUE)
  
  # fill in missing Time, Flow, PP
  loadograph$Time<-(as.numeric(loadograph$DateTime)-as.numeric(loadograph$DateTime[which(hydrograph$Time==0)]))/(60*60)
  
  loadograph$Flow<- na.approx(loadograph$Flow)
  
  EC_TP$Time<-(as.numeric(EC_TP$DateTime)-as.numeric(hydrograph$DateTime[1]))/(60*60)
  EC_TP$PP<-EC_TP$TP-EC_TP$SRP
  
  
  # get site and event names
  CNL<-nchar(FFN[i])
  sitename<-paste(substring(FFN[i], c(1,6), c(5,6)), collapse="") 
  eventname<-paste(substring(FFN[i], c(17,CNL-4), c(CNL-5,CNL-4)), collapse="") 
  
  # assign watershed area value #
  if (sitename == "AHSSUB"){DA<-14.16}
  if (sitename == "DCSSUB"){DA<-8}
  if (sitename == "DCNSUB"){DA<-4.85}
  
  
  # flow depth = flow/DA in ha times conversion factor to mm/day
  # L/s * 1/ha * 0.001 m3/L * 0.0001 ha/m2 * 1000 mm/m  * (60 s/min) * (60 min/hr)
  loadograph$Flow<-loadograph$Flow*(1/DA)*0.001*0.0001*1000*60*60*24
  
  # set start end concnetrations to 50 ug/L SRP
  matrix_TP<-50
  matrix_SRP<-50
  
  d<-dim(loadograph)
  
  loadograph$TP[1]<-matrix_TP
  loadograph$TP[d[1]]<-matrix_TP
  loadograph$SRP[1]<-matrix_SRP
  loadograph$SRP[d[1]]<-matrix_SRP
  
  loadograph$PP<-loadograph$TP-loadograph$SRP
  
  # separate dataframe for interpolate P concentrations for plotting
  interpolate_P<-loadograph
  
  interpolate_P$TP<-na.approx((interpolate_P$TP))
  
  interpolate_P$SRP<-na.approx((interpolate_P$SRP))
  
  interpolate_P$PP<-interpolate_P$TP-interpolate_P$SRP
  
 
  
  
  
  #### end ####

  #### rainfall ####
  # find 24 hours prior to flow start time and create
  # rainfall df for each event
  
  # rainfall start time
  rST<-hydrograph$DateTime[1]-86400
  d1<-dim(hydrograph)
  
  # event rain df, create time column for plotting later
  event_rain <- hourly_precip[hourly_precip$hour >= (hydrograph$DateTime[1]-86400) & hourly_precip$hour <= hydrograph$DateTime[d1[1]],]
  event_rain$Time<-(as.numeric(event_rain$hour)-as.numeric(hydrograph$DateTime[1]))/(60*60)
    
  # rain stats
  total_rain<-sum(event_rain$Event)
  
  prior_rain<-sum(event_rain$Event[event_rain$hour<hydrograph$DateTime[1]])
  
  response_time<-round(as.numeric(hydrograph$DateTime[1]-event_rain$hour[1], units = 'hours'), 2)
  
  # if response time is less than zero or NA, we know it is a snowmelt event
  if (response_time < 0 || is.na(response_time)){
    response_time<-"Snowmelt Event"
  }
  
  #### end ####
  
  #### ploting ####
  
  # scale P concentrations to flow
  my_factor<-max(loadograph$Flow, na.rm = T)/max(loadograph$TP, na.rm = T)
  
  # pivot P metrics longer 
  
  melt_P_points<-loadograph%>% pivot_longer(c(TP, SRP, PP), names_to = "Metric", values_to = 'Value') %>% mutate(Value = Value*my_factor)
        
  melt_P_interpolate<-interpolate_P%>% pivot_longer(c(TP, SRP, PP), names_to = "Metric", values_to = 'Value') %>% mutate(Value = Value*my_factor)
  
  
  
  # need rainfall dataframe: if/else statement accounts for if we have rain or no rain
  if (nrow(event_rain) != 0){
    rain_df<-event_rain[,c(3,2)]
    rain_df$name<-"hourly rainfall"
    colnames(rain_df)<-c("Time", "yrain", 'rainname')
    my_factor_rain<-(max(loadograph$Flow, na.rm = T)/max(rain_df$yrain, na.rm = T))
    rain_df$yrain<-rain_df$yrain*my_factor_rain
  } else {
    rain_df <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(rain_df)<-c("Time", "yrain", 'rainname')
  }
  
  # create plot                  
  p1<-ggplot() + 
    geom_col(rain_df, mapping = aes(x = Time, y = yrain), size = .5, color = "lightblue", fill = "white") +
    geom_line(loadograph, mapping = aes(x = Time, y = Flow)) +
    geom_line(melt_P_interpolate, mapping = aes(x=Time, y=Value, color = Metric)) +
    geom_point(melt_P_points, mapping = aes(x=Time, y=Value, color = Metric)) +
    scale_color_manual(name = "Metric", values = c("PP" = "red", "SRP" = "green", "TP" = "blue", "Flow" = "black" )) + # add flow to legend manually
    scale_y_continuous(name ="Flow (mm/day)\n", sec.axis = sec_axis(trans = ~ . / my_factor, name =  "P Concentrations (ug/L)\n")) +
    xlab("Time (hr)")+
    xlim(c(-3,32))+
    # ggtitle(paste(sitename, eventname)) +
    theme_minimal() +
    theme(legend.position="bottom") 
  
  p1
  
  #### end ####
  
  ##### save plots ####
  
  # ggsave(
  #   filename = paste0(sitename, " ", eventname, ".png"),
  #   plot = p1,
  #   path = 'C:/School/Data/plots'
  # )
  #### end ####
  
}







