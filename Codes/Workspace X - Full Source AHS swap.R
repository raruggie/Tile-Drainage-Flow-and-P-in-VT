rm(list=ls(all=T))
setwd("C:/RAR-Uvm/Data/Data for thesis")

### librarys and options ####
# library(pracma)
# library(scales)
# library(lubridate)
# library(tidyverse)
# library(zoo)
# library(car)
# library(lattice)
# library(knitr)
# library(kableExtra)
# library(lfstat)
# library(PerformanceAnalytics)
# library(ggpmisc)


# options
options(max.print = 10000)

#### end ####


#### functions ####

# Days since P application function
get_P_app_day<-function(e, s){ # input a date column
  
  # P application dates for each site
  if (s == "DCSSUB" | s == "DCNSUB"){
    P_app_date<-as.Date(c("10/12/2019", "04/27/2020", "09/28/2020", "04/15/2021", "05/24/2021", "10/02/2021"), format = "%m/%d/%Y")
  } 
  if (s == "AHSSUB"){
    P_app_date<-as.Date(c("10/06/2018", "05/19/2019", "10/06/2019", "05/05/2020", "10/06/2020"), format = "%m/%d/%Y")
  }
  
  # time difference, i.e. number of days between event and P application dates
  # this find the difference but only for the date closest 
  e<-as.Date(e)
  cd<-as.numeric(P_app_date-e) # closests date, refine in next step
  cd<-cd[cd<0] # only want negative times
  cd<-max(cd) # want the maximum ofr the negative times, i.e. the cloests date prior
  # the time difference is just the abosulte value of the closests date
  td<-abs(cd)
  
  return(td)
  
}

# Days since just manure application function
get_manure_app_day<-function(e, s){ # input a date column
  
  # P application dates for each site
  if (s == "DCSSUB" | s == "DCNSUB"){
    P_app_date<-as.Date(c("10/12/2019", "09/28/2020", "10/02/2021"), format = "%m/%d/%Y")
  } 
  if (s == "AHSSUB"){
    P_app_date<-as.Date(c("10/06/2018","10/06/2019","10/06/2020"), format = "%m/%d/%Y")
  }
  
  # time difference, i.e. number of days between event and P application dates
  # this find the difference but only for the date closest 
  e<-as.Date(e)
  cd<-as.numeric(P_app_date-e) # closests date, refine in next step
  cd<-cd[cd<0] # only want negative times
  cd<-max(cd) # want the maximum ofr the negative times, i.e. the cloests date prior
  # the time difference is just the abosulte value of the closests date
  td<-abs(cd)
  
  return(td)
  
}

# Add Four hydrologic season and day of hydrologic year using function
getSeason_hydrologic <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,331,630,930,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall")
  return(cuts)
}

# G vs NG seasons 
getSeason<-function(d){
  growing<-c(5,6,7,8,9)
  if (as.numeric(format(d,"%m")) %in% growing) {
    s<-"Growing"
  }
  if (!as.numeric(format(d,"%m")) %in% growing) {
    s<-"Non-Growing"
  }
  return(s)
}

# copied from USGS github lfstats package
get_waterYearDay <- function(x) {
  
  year_day <- lubridate::yday(x)
  yrs_leap <- lubridate::leap_year(x)
  
  # October 1st (day 1 of water year) is day 274 (or 275 in leap year) of calendar year.
  # (274 + years_leap) == 275 for dates in leap year and 274 for dates not in leap year.
  # This provides a boolean selector for days before (false) / after (true) Oct 1.
  after_waterday <- year_day >= (274 + yrs_leap)
  
  # 273 (or 274 in leap year) days from January 1 to October 1
  # This gives us 1 for October 1st and corrects everything till December 31st.
  year_day[after_waterday] <- year_day[after_waterday] - (273 + yrs_leap[after_waterday])
  
  # 92 days from October 1 to January 1
  # This gives us 93 for January 1 and corrects everything till September 29th.
  year_day[!after_waterday] <- year_day[!after_waterday] + 92
  
  return(year_day)
  
}

#### end funs ####

#### event data ####

# read in file names in the working directory

temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

# now you have names of files as a matrix in 'temp' and the data in 'myfiles'
# separate out flow and EC,TP data for each site
# add names to lists from temp

Conc<-myfiles[grepl("EC, TP", temp)]; names(Conc)<-temp[grepl("EC, TP", temp)]
Flow<-myfiles[grepl("Flow", temp)]; names(Flow)<-temp[grepl("Flow", temp)]

# there are a bunch of hydrographs that don't match up with discrete conc data
# so remove them from 'Flow' so you have ordered paired of matching events
# save them for use after getting load data situated for the discrete events

# create vector of flow file names that have discrete data (i.e. a EC,TP paired file)
n<-names(Conc)
n<-gsub("EC, TP", "Flow", n) # need to swap out 'EC, TP' for 'Flow' to get flow file names that match conc data

Flow_rest<-Flow[!names(Flow) %in% n] # reduce all hydrographs to just ones that don't have 
Flow_save<-Flow # save a flow for later
Flow<-Flow[n]; # write over large flow with smaller flow using names from conc data as index

## Clean up dataframes using a function

# Flow
cleanFlow<-function(DF){
  
  # remove any lingering columns
  DF<-DF[,-3]
  
  # rename columns
  colnames(DF)<-c("DateTime", "Flow")
  
  # Some files have two digit dates (files prior to 05062021)
  # to account for this, first run all (including two digit) dates into POSIXlt
  # iF result is NA, then you know it is a 4 digit 
  test_D <- as.POSIXlt(DF$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
  
  if (is.na(test_D[1])) {
    DF$DateTime <- as.POSIXlt(DF$DateTime, format="%m/%d/%Y %H:%M", tz = 'UTC')
  } else {
    DF$DateTime <- as.POSIXlt(DF$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
  }
  
  # create time column
  DF$Time<-(as.numeric(DF$DateTime)-as.numeric(DF$DateTime[1]))/(60*60)
  
  # omit NA's
  DF<-na.omit(DF)
  
  # return cleaned up dataframe
  return(DF)

}
Flow<-lapply(Flow, cleanFlow)
Flow_rest<-lapply(Flow_rest, cleanFlow)
Flow_save<-lapply(Flow_save, cleanFlow)

# Concentration
cleanConc<-function(DF){
  
  # Some of the AHS concentration data does not have SRP, thus have 4 columns and want a uniform 5
  if (ncol(DF) == 4){
    DF<-tibble::add_column(DF, SRP = NA, .after = 3)
  }
  
  # Uniform column names
  colnames(DF)<-c("DateTime", "EC..mS.", "TP", "SRP", "Sample.Number")
  
  # Convert sample number to character
  DF$Sample.Number<-as.character(DF$Sample.Number)
  
  # 2 and 4 digit year combos in input files
  test_D <- as.POSIXlt(DF$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
  
  if (is.na(test_D[1])) {
    DF$DateTime <- as.POSIXlt(DF$DateTime, format="%m/%d/%Y %H:%M", tz = 'UTC')
  } else {
    DF$DateTime <- as.POSIXlt(DF$DateTime, format="%m/%d/%y %H:%M", tz = 'UTC')
  }
  
  # Calculate PP
  DF<-tibble::add_column(DF, PP = DF$TP-DF$SRP, .after = 4)
  
  # return cleaned up dataframe
  return(DF)
  
}
Conc<-lapply(Conc, cleanConc)

## Remove Samples that occur before and after event end using function
# this uses a counter to match dataframe in Conc list to dataframe in Flow list
# since (I only know how to insert) one list goes into lapply

counter <- 0
RB<-function(DF){ # RB = 'remove bottles'
  counter<<- counter + 1
  # remove bottles after event
  DF<-DF[which(DF$DateTime < Flow[[counter]]$DateTime[nrow(Flow[[counter]])]),]
  # remove bottles before event
  DF<-DF[which(DF$DateTime > Flow[[counter]]$DateTime[1]),]
  return(DF)
  
}
Conc<-lapply(Conc, RB) # note if you do not reset counter back to zero, it will pick up at last value (i.e. 59, which is out of bounds for list)

## Loadographs and rainfall pulse:

# rainfall prior loop
rainfall<-read.csv(file= ".Tips_10042019-08022021.csv")
rainfall$DateTime<-as.POSIXct(rainfall$DateTime, format="%m/%d/%y %H:%M")
rainfall$Event<-0.01*2.54*10
RB <- aggregate(rainfall['Event'], list(hour=cut(as.POSIXct(rainfall$DateTime), "30 min")), sum) # cut by half hour, RB = rainfall bins
RB$hour<-as.POSIXct(RB$hour, format="%Y-%m-%d %H:%M:%S", tz = 'UTC')
RB$hour<-round_date(RB$hour, "5 min") # round to closest 5 minutes so it matches with flow data
Q<-as.numeric(quantile(RB$Event*2*24)) # rainfall quartiles in units mm/day, assigned to dataframe, used later in main loop

# event slow flow recession constants prior loop (from individual site event hydrograph analysis prior)
SFRC_1<-c(          # values prior to 04/16/2021
          -0.108,-0.015, # DCS,DCN 0512  
          -0.055, # DCN 1201
          -0.08, # DCN 1225
          -0.01896,# DCN 0117
          -0.00352,# AHS 0117
          -0.055, -0.0414 # DCS,DCN 0311
)
SFRC_2<-read.csv(file = '.DC IEA RC Spring Summer 2021.csv')# newer RCs
SFRC_2<-c(SFRC_2[-c(17,18, 21,22, 23,24), 6]) # remove events with no baseflow EC, remove columns other than SF RCsS
SFRC<-c(SFRC_1, SFRC_2) # combine vectors

# baseflow prior to events prior loop
bec<-read.csv(file= ".Baseflow EC prior to events.csv")
bec$Event<-as.character(bec$Event) # convert to character to check if zeros are needed at start of event name
bec$Event[which(nchar(bec$Event) == 7)] <- paste("0", bec$Event[which(nchar(bec$Event) == 7)], sep="") # where number of characters are 7, add zero to start of string
bec$DateTime<-paste(bec$Event,'0:0:0')
bec$DateTime<-as.POSIXlt(bec$DateTime, format = '%m%d%Y %H:%M:%S', tz = 'UTC')
colnames(bec)<-c("Event", "EC..mS.", "DateTime")

# list of events in 4 component hydrograph separation prior to loop
FFN<-c('DCSSUB - Flow - 05122020.csv','DCNSUB - Flow - 05122020.csv','DCNSUB - Flow - 12012020.csv','DCNSUB - Flow - 12252020.csv',
       'DCNSUB - Flow - 01172021.csv','AHSSUB - Flow - 01172021.csv','DCSSUB - Flow - 03112021.csv','DCNSUB - Flow - 03112021.csv',
       'DCSSUB - Flow - 04162021.csv','DCNSUB - Flow - 04162021.csv','DCSSUB - Flow - 04222021.csv','DCNSUB - Flow - 04222021.csv',
       'DCSSUB - Flow - 04302021.csv','DCNSUB - Flow - 04302021.csv','DCSSUB - Flow - 05062021.csv','DCNSUB - Flow - 05062021.csv',
       'DCSSUB - Flow - 07022021.csv','DCNSUB - Flow - 07022021.csv','DCSSUB - Flow - 07032021.csv','DCNSUB - Flow - 07032021.csv',
       'DCSSUB - Flow - 07092021.csv','DCNSUB - Flow - 07092021.csv','DCSSUB - Flow - 07132021.csv','DCNSUB - Flow - 07132021.csv',
       'DCSSUB - Flow - 07182021.csv','DCNSUB - Flow - 07182021.csv'
)

# merge FFN with baseflow prior and SFRC prior to loop
FFN.SFRC<-data.frame(bec$Event,FFN,SFRC)
FBS<-cbind(FFN.SFRC, bec[,2])
colnames(FBS)<-c(names(bec)[1], names(FFN.SFRC)[2:3], names(bec)[2])

# append out dataframes
Total<-data.frame()
bottles<-data.frame()
dat<-data.frame() # rainfall
dat_EC<-data.frame() # EC, large table of 4 comp model events
hydrograph_plots<-list()
facet_plots<-list()

# indexs of loop interations that are not included in four comp so can remove them by index value in list of ggplots after loop
rm<-0


# loop for merging Flow and discrete concentration data
i<-55
for (i in seq(1:length(Flow))){

  # Site, event names and then getting draiange areas, baseflow TP
  CNL<-nchar(names(Flow)[i])
  sitename<-paste(substring(names(Flow)[i], c(1,6), c(5,6)), collapse="")
  eventname<-paste(substring(names(Flow)[i], c(17,CNL-4), c(CNL-5,CNL-4)), collapse="")

  if (sitename == "AHSSUB"){DA<-14.16; mp<-15}
  if (sitename == "DCSSUB"){DA<-8; mp<-50}
  if (sitename == "DCNSUB"){DA<-4.85; mp<-50}

  # merge - you like this merge because if samples occur on a 5 minute clock time (i.e. same as flow data), it doesn't produce another entry, as if you were to do a different merge (e.g. by.x = "DateTime, Sitename), you would get duplicates
  SEM<-merge(x = Flow[[i]],y = Conc[[i]], by.x = "DateTime", by.y = "DateTime", all = TRUE)

  #### Total ####
  
  # peak TP and SRP concentrations
  peak.TP.Conc<-max(SEM$TP, na.rm = T)
  peak.TP.Conc<-ifelse(is.finite(peak.TP.Conc), peak.TP.Conc, 0) # if all are NA, returns -Inf, so want to set to zero in that case
  peak.SRP.Conc<-max(SEM$SRP, na.rm = T)
  peak.SRP.Conc<-ifelse(is.finite(peak.SRP.Conc), peak.SRP.Conc, 0) # if all are NA, returns -Inf, so want to set to zero in that case
  
  
  # fill in missing Time and Flow where samples did not occur on a 5 minute clock time
  SEM$Time<-(as.numeric(SEM$DateTime)-as.numeric(SEM$DateTime[1]))/(60*60)
  SEM$Flow<- na.approx(SEM$Flow)
  
  # sample bottles
  b<-SEM[complete.cases(SEM$TP),]
  b<-b%>%mutate(Flow = Flow*(1/1000)*(1/DA)*(1/10000)*(1000)*(60*60*24), Event = as.Date(DateTime), Site = sitename) %>% # flow rate in mm/day
        dplyr::select(c(Event,Site,Time,Flow,TP,SRP,PP))
  bottles<-rbind(bottles,b)
  
  
  ## create LOADOGRAPH
  l<-nrow(SEM)
  SEM$SRP<-as.numeric(SEM$SRP) # is there is no SRP values, class is logical, which gives error when doing trapz

  SEM$TP[c(1,l)]<-mp; SEM$TP<-na.approx((SEM$TP)) ## set pre-post event values; mp is matrix P set in sitename if statements above; TP simple because all events have TP

  if (sum(complete.cases(SEM$SRP)) != 0){  # some events have no SRP data, so I want to keep the NA's
    SEM$SRP[c(1,l)]<-mp; SEM$SRP<-na.approx((SEM$SRP))
    SEM$PP[c(1,l)]<-0; SEM$PP<-na.approx((SEM$PP))
  }

  # convert chemograph into loadograph simply by multiplying by the flowrate associatewith that datapoint; convert ug to g/s here
  SEM<-SEM%>%mutate(TP = (1/1000000)*TP*Flow, SRP = (1/1000000)*SRP*Flow, PP = (1/1000000)*PP*Flow)

  # save SEM df at this point for rain pulse later in loop 
  SEM_save<-SEM
  
  # integrate loadographs to get units ug, convert to g/ha
  TPL<-(trapz(as.numeric(SEM$DateTime), SEM$TP))*(1/DA) # g/ha
  SRPL<-(trapz(as.numeric(SEM$DateTime), SEM$SRP))*(1/DA)

  # Event loads, volumes and peak P concentrations/FWMC
  EV<-(trapz(as.numeric(SEM$DateTime), SEM$Flow))/1000 # m3
  EV_mm<-EV*(1/DA)*(1/10000)*(1000) # mm

  ## FWMC back-calcs

  samples<-c(as.numeric(rownames(SEM[complete.cases(SEM$EC..mS.),])), nrow(SEM)) # find volume between bottlesalso, because you use entire event volume here, assume end of hydrograph is a sample
  samples_df<-as.data.frame(samples) # df of rownames

  SEM$TPgBB<-NA # grams between bottles
  SEM$TPgA<-NA # grams accumulated, don't need but can use as check
  SEM$SRPgBB<-NA

  for (j in samples){
    k<-as.numeric(rownames(samples_df)[samples_df$samples==j]) # this is just used to start the process, k equal 1 in the first loop iteration, and the if statement catches that condition
    if (k == 1){ # first row (k = 1) is start, j is end for gBB/gA
      SEM$TPgBB[j]<-trapz(as.numeric(SEM$DateTime[k:j]), SEM$TP[k:j])
      SEM$SRPgBB[j]<-trapz(as.numeric(SEM$DateTime[k:j]), SEM$SRP[k:j])
      SEM$TPgA[j]<-SEM$TPgBB[j]
    }
    else{
      s<-samples[k-1] # s is start, j is end
      SEM$TPgBB[j]<-trapz(as.numeric(SEM$DateTime[s:j]), SEM$TP[s:j])
      SEM$SRPgBB[j]<-trapz(as.numeric(SEM$DateTime[s:j]), SEM$SRP[s:j])
      SEM$TPgA[j]<-SEM$TPgBB[j]+SEM$TPgA[s]
    }
  }

  # FWMC in ug/L
  SEM[is.na(SEM)] <- 0
  FWMC_TP<-(sum(SEM$TPgBB)*1000000)/(EV*1000)
  FWMC_SRP<-(sum(SEM$SRPgBB)*1000000)/(EV*1000)

  # append to total dataframe
  out<-data.frame(Site = sitename, Event = eventname, Volume = EV_mm, TP.Load = TPL, TP.Conc = FWMC_TP, SRP.Load = SRPL, SRP.Conc = FWMC_SRP, Peak.TP.Conc = peak.TP.Conc, Peak.SRP.Conc = peak.SRP.Conc)
  Total<-rbind(Total, out)
  
  #### end Total ####
  
  #### rainfall pulse ####
  rp<-SEM_save%>% # note P in g/s, want g/ha/day
        mutate(TP = TP*(60*60*24)*(1/DA), SRP = SRP*(60*60*24)*(1/DA), PP = PP*(60*60*24)*(1/DA))
  
  
  d1<-dim(Flow[[i]])
  
  # event rain df, create time column for plotting later
  event_rain <- RB[RB$hour >= (Flow[[i]]$DateTime[1]-86400) & RB$hour <= Flow[[i]]$DateTime[d1[1]],]
  event_rain$Time<-(as.numeric(event_rain$hour)-as.numeric(Flow[[i]]$DateTime[1]))/(60*60)
  
  # rain stats
  total_rain<-sum(event_rain$Event)
  prior_rain<-sum(event_rain$Event[event_rain$hour<Flow[[i]]$DateTime[1]])
  response_time<-as.numeric(Flow[[i]]$DateTime[1] - event_rain$hour[1])
  
  # if response time is less than zero or NA, we know it is a snowmelt event and need to account for that
  if (response_time < 0 || is.na(response_time)){
    pulse_df<-data.frame(matrix(ncol = 12, nrow = 0))
    colnames(pulse_df)<-c("pulse_endtime","pulse_endTime","pulse_length", "pulse_sums", "pulse_max", "Q", "pSRP","SRP_loado", "PP_loado","maxFlow","site","event")
  } else {
    ## rain intensity: each pulse volume is how much rain falls in consecutive hours, thus when an hour has zero total rainfall, the pulse is over and the next pulse can start
    
    # create dataframe with time going from start of rainfall to end of hydrograph, EVERY HOUR
    # event_time <- data.frame(DateTime = seq(event_rain$hour[1], as.POSIXct(hydrograph$DateTime[d1[1]]), by = ((60*60))))
    
    # create dataframe with time going from start of rainfall to end of hydrograph, EVERY HALF HOUR
    event_time <- data.frame(DateTime = seq(event_rain$hour[1], as.POSIXct(Flow[[i]]$DateTime[d1[1]]), by = ((60*60)/2)))
    
    # merge to get hourly rainfall intensities throughout the entire event time period
    RI <- merge(x = event_time,y = event_rain, by.x = "DateTime", by.y = "hour", all = TRUE)
    
    RI$Time<-(as.numeric(RI$DateTime)-as.numeric(Flow[[i]]$DateTime[1]))/(60*60)
    
    # add hour counter to determine how long each pulse is later
    RI$H<-0.5 # 0.5 for 1/2 hour aggregated rainfall
    
    # calculate unique pulse blocks
    (pulses = cumsum(c(0,diff(complete.cases(RI)) != 0 )))
    
    # calculate pulse block stats
    (pulse_sums = as.numeric(tapply(RI$Event,pulses,sum))) # mm
    (pulse_max = as.numeric(tapply(RI$Event,pulses,max))*2*24) # from mm/30 minutes to mm/day
    (pulse_length<-as.numeric(tapply(RI$H,pulses,sum))) # hours
    (pulse_endtime<-as.POSIXct(as.numeric(tapply(RI$DateTime,pulses,max)), origin = "1970-01-01", tz = 'UTC'))
    
    (pulse_endTime<-(as.numeric(pulse_endtime)-as.numeric(Flow[[i]]$DateTime[1]))/(60*60))
    
    # create pulse block dataframe
    pulse_df<-data.frame(pulse_endtime, pulse_endTime, pulse_length, pulse_sums, pulse_max)
    
    # create qunatile column
    pulse_df$Q<-NA
    
    # assign pulse quantile info
    dp<-dim(pulse_df)
    
    k<-1
    
    for (k in seq(1:dp[1])){
      if (is.na(pulse_df$pulse_max[k])){
        pulse_df$Q[k]<-"None"
      }
      if (!is.na(pulse_df$pulse_max[k])) {
        if (pulse_df$pulse_max[k] <= Q[4]){
          pulse_df$Q[k] <-'Low'
        }
        if (pulse_df$pulse_max[k] > Q[4]){
          pulse_df$Q[k] <- 'High'
        }
      }
    }
    
    # make pulse start time variable
    (pulse_starttime<-pulse_df$pulse_endtime-(pulse_df$pulse_length*60*60))
    (pulse_startTime<-(as.numeric(pulse_starttime)-as.numeric(Flow[[i]]$DateTime[1]))/(60*60))
    
    # set negative sTart times to zero
    pulse_startTime[pulse_startTime<0]<-0 # set negative start times (i.e. the first in many cases) to zero
    
    # assign desired metrics 
    pulse_df$meanTP<-NA
    pulse_df$meanSRP<-NA
    pulse_df$meanPP<-NA
    pulse_df$maxFlow<-NA
    
    ## make flow from L/s to mm/day
    
    # L/s to mm/day
    rp$Flow<-rp$Flow*(1/DA)*0.001*0.0001*1000*60*60*24
    
    # save loadograph point at sample bottles concnetrations
    sc<-rp[complete.cases(rp$EC..mS.),]
  
    l<-1
    
    # this is the loop you change mean to max and end time to start time to switch things ****
    for (l in seq(1,dp[1]-1)){ # go from first row to second to last row since all events will have last block as none and it throws error if you try to grab the last hour plus one more
      if (pulse_endtime[l] > rp$DateTime[1]){ # need to account for pulses that end prior to the start of nutrient data
        if ((pulse_endtime[l] + 3600) < rp$DateTime[nrow(rp)]) { # need to account for pulses that go past the end of the event
          if (l == 1){ # need to account for first row interpolation issues
            # here assumed that 
            pulse_df$meanTP[l]<-sc$TP[1]
            pulse_df$meanSRP[l]<-sc$SRP[1]
            pulse_df$meanPP[l]<-sc$PP[1]
            pulse_df$maxFlow[l]<-max(rp$Flow[1:rownames(rp)[which(rp$DateTime == pulse_endtime[1]+3600)]])
          }
          else {
            if (pulse_starttime[l] < rp$DateTime[1]){
              sr <-0
            }
            else {
              sr<-rownames(rp)[max(which(rp$DateTime <= pulse_starttime[l] +3600))] # start row
            }
            er<-rownames(rp)[min(which(rp$DateTime > pulse_endtime[l]+3600))] # end row
            pulse_df$meanTP[l]<-mean(rp$TP[sr:er])
            pulse_df$meanSRP[l]<-mean(rp$SRP[sr:er])
            pulse_df$meanPP[l]<-mean(rp$PP[sr:er])
            pulse_df$maxFlow[l]<-max(rp$Flow[sr:er])
          }
        }
      }
    }
    
    # remove rows with no pulses
    pulse_df<-na.omit(pulse_df)
    dp<-dim(pulse_df)
    
    # add name of site-event
    # need to account for if event contained all pulses that ended prior to the start of nutrient data, see first if statement in for loop above
    if (dim(pulse_df)[1] != 0){
      pulse_df$site<-sitename
      pulse_df$event<-eventname
    }
    
    #### end ####
  }
  
  # rbind pulse dataframe to output df 
  dat<-rbind(dat, pulse_df[])
  
  # 24 hour antecedent rainfall for event, PR = prior rain
  AR_24h <- RB[RB$hour >= (Flow[[i]]$DateTime[1]-86400) & RB$hour <= Flow[[i]]$DateTime[d1[1]],]
  PR_24h <- sum(AR_24h$Event[AR_24h$hour<Flow[[i]]$DateTime[1]])
  
  # 7 day antecdent rainfall for event, PR = prior rain
  AR_7D <- RB[RB$hour >= (Flow[[i]]$DateTime[1]-604800) & RB$hour <= Flow[[i]]$DateTime[d1[1]],]
  PR_7D <- sum(AR_7D$Event[AR_7D$hour<Flow[[i]]$DateTime[1]])
  
  # 30 day antecdent rainfall for event, PR = prior rain
  AR_30D <- RB[RB$hour >= (Flow[[i]]$DateTime[1]-2592000) & RB$hour <= Flow[[i]]$DateTime[d1[1]],]
  PR_30D <- sum(AR_30D$Event[AR_30D$hour<Flow[[i]]$DateTime[1]])
  
  #### end rainfall pulse ####
  
  #### hydrograph separations ####
  
  comp4<-SEM_save%>%
    mutate(Flow = Flow * (1/DA)*0.001*0.0001*1000*60*60*24) # flow in mm/day
  # Only perform the code below if Flow[[i]] falls in the events that are included in the analysis
  
  
  if (names(Flow)[i] %in% FFN & sitename != "AHSSUB"){
    
    # Recession Analysis 
    SFRC.i<-FBS$SFRC[FBS$FFN == names(Flow)[[i]]] # get slow flow recession constant
    comp4$SF<-NA
    comp4$SF<-comp4$Flow[1] + (abs(SFRC.i) * comp4$Time)
    comp4$SF[which(comp4$SF>comp4$Flow)]<-comp4$Flow[which(comp4$SF>comp4$Flow)]
    comp4$QF<-comp4$Flow-comp4$SF
    time_days<-as.numeric(comp4$DateTime)/(60*60*24) # convert x-axis (time) from seconds to days and save as variable. this gets used in integrations since flow compoenents are in mm/day
    QF<-(trapz(time_days, comp4$QF)) # mm
    SF<-(trapz(time_days, comp4$SF)) 
    QF_TtP<-comp4$Time[which.max(comp4$QF)]
    SF_TtP<-comp4$Time[which.max(comp4$SF)]
    
    ## EC-EMMA
    
    new_water_EC<-0.145
    old_water_EC<-FBS$EC..mS.[FBS$FFN == names(Flow)[[i]]]
    d<-dim(comp4)
    comp4$EC..mS.[1]<-old_water_EC
    comp4$EC..mS.[d[1]]<-old_water_EC
    comp4$EC..mS.<- na.approx(comp4$EC..mS.)
    
    # account for 08052020 event that had no flow before, i.e. if old_water_EC = 0
    if (old_water_EC == 0){
      comp4$new<-comp4$Flow
      comp4$old<-0
    }
    if (old_water_EC > 0){
      comp4$new<-(comp4$Flow*(comp4$EC..mS.-old_water_EC))/(new_water_EC-old_water_EC)
      comp4$old<-comp4$Flow-comp4$new
    }
    
    new<-(trapz(time_days, comp4$new)) # mm
    old<-(trapz(time_days, comp4$old)) 
    new_TtP<-comp4$Time[which.max(comp4$new)]
    old_TtP<-comp4$Time[which.max(comp4$old)]
    
    # four compoent hydrograph separation
    comp4$QF_old<-NA
    comp4$QF_new<-NA
    comp4$SF_old<-NA
    comp4$SF_new<-NA
    
    for (k in 1:nrow(comp4)){
      if (comp4$QF[k] >= comp4$new[k]){
        comp4$QF_old[k]<-comp4$QF[k]-comp4$new[k]
        comp4$QF_new[k]<-comp4$new[k]
        comp4$SF_old[k]<-comp4$Flow[k]-comp4$QF[k]
        comp4$SF_new[k]<-0
      }
      else if (comp4$QF[k] < comp4$new[k]) {
        comp4$QF_old[k]<-0
        comp4$QF_new[k]<-comp4$QF[k]
        comp4$SF_old[k]<-comp4$Flow[k]-comp4$new[k]
        comp4$SF_new[k]<-comp4$new[k]-comp4$QF[k]
      }
    }
    
    QF_old<-(trapz(time_days, comp4$QF_old)) # mm
    QF_new<-(trapz(time_days, comp4$QF_new)) 
    SF_old<-(trapz(time_days, comp4$SF_old)) 
    SF_new<-(trapz(time_days, comp4$SF_new)) 
    QF_old_TtP<-comp4$Time[which.max(comp4$QF_old)]
    QF_new_TtP<-comp4$Time[which.max(comp4$QF_new)]
    SF_old_TtP<-comp4$Time[which.max(comp4$SF_old)]
    SF_new_TtP<-comp4$Time[which.max(comp4$SF_new)]
    
    
    # plots
    
    # set maximum y limits for hydrograph and facet plots;
    # max flow for DCSSUB is 37 mm/day, 60 mm/hr for DCNSUB
    if (sitename == 'DCSSUB'){
      maxf<-1.55*24
      maxP<-1500
    }
    if (sitename == 'DCNSUB'){
      maxf<-2.5*24
      maxP<-1500
    }
    if (sitename == 'AHSSUB'){ 
      maxf<-4
      maxP<-200
    }
    
    ## hydrograph plots
  
    loado_mm_day_melt <- comp4 %>% 
      pivot_longer(c(Flow, QF_new, QF_old, SF_new, SF_old), names_to = 'Metric_Flow', values_to = 'Value_Flow')
    
    p1<-ggplot(data = loado_mm_day_melt) +
      geom_line(aes(y = Value_Flow, x = Time, color = Metric_Flow), lwd=1.2) +
      labs(colour = "Hydrograph\nComponent") +
      xlab("\nTime (hr)") + 
      ylab("Flow (mm/day)\n")+
      ylim(0,maxf)+
      theme_minimal(base_size = 7)+
      theme(axis.text.x = element_text(angle=45, hjust=1))+
      ggtitle(paste(sitename, eventname))
    
    hydrograph_plots[[i]]<-p1
    names(hydrograph_plots)[[i]]<-paste0(sitename, as.Date(Flow[[i]]$DateTime[1]))
    
    ## facet plot for P speices, not by 4 comp but just concentrations in bottles
    
    EC_TP<-Conc[[i]]%>%
      dplyr::select(-Sample.Number)%>%
      mutate(DateTime = round_date(DateTime, "5 minutes"))%>%
      mutate(Flow = inner_join(Flow[[i]], .)$Flow)%>%
      select(c(Flow, TP, SRP, PP))%>%
      pivot_longer(c(TP, SRP, PP), names_to = 'Metric_P', values_to = 'Value_P')
    
    # want to account for 12/25/2020 event when SRP was high
    if (eventname == '12252020'){
      maxP<-max(Conc[[i]]$TP, na.rm = T)
    }
    
    # max flow should be event specific for this plot
    maxf<-max(comp4$Flow, na.rm = T)
    
    p2<-ggplot(data = EC_TP) +
      geom_point(aes(y = Value_P, x =Flow, color = Metric_P)) +
      labs(colour = "P Species") +
      # facet_wrap(~Metric_P) +
      xlab("\nFlow (mm/day)") +
      ylab("P Concentration (ug/L)\n")+
      xlim(0,maxf)+
      ylim(0,maxP)+
      theme_minimal(base_size = 7)+
      theme(axis.text.x = element_text(angle=45, hjust=1))+
      ggtitle(paste(sitename, eventname))
    
    facet_plots[[i]]<-p2
    names(facet_plots)[[i]]<-paste0(sitename, as.Date(Flow[[i]]$DateTime[1]))
    
    # write out to dataframe
    RR<-EV_mm/total_rain
    mhri<-max(event_rain$Event)*10*24
    ttph<-min(as.numeric(Flow[[i]]$DateTime[which(Flow[[i]]$Flow == max(Flow[[i]]$Flow))])-as.numeric(Flow[[i]]$DateTime[1]))/(60*60)
    maxFlow<-max(Flow[[i]]$Flow, na.rm = T)
    PPL<-TPL-SRPL
    dsMa<-get_manure_app_day(Flow[[i]]$DateTime[1], sitename)
    dsPa<-get_P_app_day(Flow[[i]]$DateTime[1], sitename)
    Season<-getSeason(Flow[[i]]$DateTime[1])
    Season4<-getSeason_hydrologic(Flow[[i]]$DateTime[1])
    
    
    df.1<-data.frame(eventname,sitename,Season,Season4,EV_mm,total_rain,RR,prior_rain,PR_7D,PR_30D,
                     mhri,response_time,ttph,maxFlow,TPL,SRPL,PPL,dsMa,dsPa,
                     QF,QF_TtP,SF, SF_TtP,old,old_TtP,new,new_TtP,
                     QF_old,QF_old_TtP,QF_new,QF_new_TtP,SF_old,SF_old_TtP,SF_new,SF_new_TtP)
    
    dat_EC<-rbind(dat_EC, df.1)
  } # end if statement wrapper for 4 comp events in main loop
  else {
    rm<-c(rm,i)
  }
}

## clean dataframes and write to workspace

# rainfall pulse analysis - filter pulses that fall outside of range for legacy P/transport limitation
rainfall_pulse<-dat%>%
  rowwise()%>%
  mutate(Day.since.P = get_P_app_day(pulse_endtime, site), Day.since.Manure = get_manure_app_day(pulse_endtime, site))%>%
  filter(Day.since.Manure>100)%>%
  dplyr::select(c(Q, pulse_length,pulse_sums,pulse_max,meanTP,meanSRP,meanPP,maxFlow))

# hydro and facet ggplots
rm<-rm[-1]
hydrograph_plots<-rlist::list.remove(hydrograph_plots, rm)
facet_plots<-rlist::list.remove(facet_plots, rm)

hydrograph_plots <- hydrograph_plots[order(names(hydrograph_plots))]
facet_plots <- facet_plots[order(names(facet_plots))]

# 4 comp dataframe
EC<-dat_EC%>%
  mutate(Event = as.Date(eventname, format = "%m%d%Y"),
         Site = sitename, 
         sitexevent = paste0(sitename,eventname))%>%
  arrange(Event, Site)%>%
  select(Event, Site, sitexevent, everything(), -c(sitename, eventname))

dfs<-Filter(function(x) is.list(get(x)) , ls())

dfs

dfs<-dfs[c(12,22,16,30)] # EC, hydrograph and facet plots, and rainfall_pulse

save(list=dfs, file="work1.RData")


# loop for merging rest of Flow to Total

Flow<-Flow_rest # re-use the name "Flow": write over Flow to reduce writing out '_rest' each time in loop

for (i in seq(1:length(Flow))){
  # Site, event names and draiange areas
  CNL<-nchar(names(Flow)[i])
  sitename<-paste(substring(names(Flow)[i], c(1,6), c(5,6)), collapse="") 
  eventname<-paste(substring(names(Flow)[i], c(17,CNL-4), c(CNL-5,CNL-4)), collapse="")
  if (sitename == "AHSSUB"){DA<-14.16}
  if (sitename == "DCSSUB"){DA<-8}
  if (sitename == "DCNSUB"){DA<-4.85}
  
  # calculate volumes, integrate hydrograph
  EV<-(trapz(as.numeric(Flow[[i]]$DateTime), Flow[[i]]$Flow))/1000 # m3
  EV_mm<-EV*(1/DA)*(1/10000)*(1000) # mm
  
  out<-data.frame(Site = sitename, Event = eventname, Volume = EV_mm, TP.Load = NA, TP.Conc = NA, SRP.Load = NA, SRP.Conc = NA, Peak.TP.Conc = NA, Peak.SRP.Conc = NA)
  
  Total<-rbind(Total, out)
  
}


# clean up Total Dataframe
Total<-Total %>% mutate(Event = as.Date(Event, format = "%m%d%Y"))%>% dplyr::arrange(Site, Event) %>% mutate_at(3:6, round, 1)



# Add pre-post thesis Dead Creek tile data 

prior<-read.csv(file = ".Dead Creek FWMC data prior to and after discrete.csv")

prior<-prior %>% 
  mutate(Event = as.Date(Event, format = "%m/%d/%Y"), Site = as.factor(Site)) %>% # format date and site columns
  dplyr::mutate_if(is.character, as.numeric) %>% # everything else to numeric, convert to NA if character 
  arrange(Event) # order by date


# change levels of site to match names in thesis
levels(prior$Site)<-c("DCNSUR", "DCNSUB", "DCSSUR", "DCSSUB")

# clean up
prior<-prior[prior$Site == "DCSSUB" | prior$Site == "DCNSUB",]
prior<-prior%>%mutate(Volume = NA, TP.Load = NA, SRP.Load = NA, Peak.TP.Conc = NA, Peak.SRP.Conc = NA)
prior<-prior[,c(2,1,5,6,3,7,4,8,9)]
colnames(prior)<-c(colnames(Total))


# append prior to Total
Total<-full_join(Total, prior)%>%arrange(Site, Event)

# merge rows that have duplicate site-events, but volumes comes from Tot
# and P data came from prior
# note this works because dup;icates follow pattern:
# volumes were the first of the duplicated rows, concnetrations data the second

# combine site and event to be able to test for duplicates
Total<-Total %>%
  mutate(site.event = paste0(Site,Event))%>%
  group_by(site.event)

# r= row numbers that are duplicates, will be removed after loop
r<-0
for (i in seq(2,length(Total$Site))){
  if (Total$site.event[i] == Total$site.event[i-1]){
    Total$TP.Conc[i-1] <- Total$TP.Conc[i]
    Total$SRP.Conc[i-1] <- Total$SRP.Conc[i]
    r<-c(r,i)
  }
}
Total<-Total[-r,] # remove rows thats data has been appended to row above
Total<-Total[,-length(Total)] # remove site.event dummy col


# find P loads for events missing it
for (i in seq(1,length(Total$Site))){
  if (Total$Site[i] == "DCSSUB") {
    DA<-8
  }
  if (Total$Site[i] == "DCNSUB"){
    DA<-4.85
  }
  if (!complete.cases(Total$TP.Load[i])){
    Total$TP.Load[i]<-Total$TP.Conc[i]*Total$Volume[i]*10000*(1/1000000)
  }
  if (!complete.cases(Total$SRP.Load[i])){
    Total$SRP.Load[i]<-Total$SRP.Conc[i]*Total$Volume[i]*10000*(1/1000000)
  }
}


Total<-Total %>% rowwise() %>% mutate(Day.since.P = get_P_app_day(Event, Site), Day.since.Manure = get_manure_app_day(Event, Site))


# assign seasons
growing<-c(5,6,7,8,9) # month numbers of growing season

Total$Season<-NA
Total$Season[as.numeric(format(Total$Event,"%m")) %in% growing]<-"Growing"
Total$Season[!as.numeric(format(Total$Event,"%m")) %in% growing]<-"Non-Growing"

# add water year
Total<-Total%>%mutate(WY = lfstat::water_year(Event, origin = 'usgs'))

# add event rainfall

# load data: tb = tipping bucket
tb<-read.csv(file= ".Tips_10012018-10302021.csv")

# account for two digit dates
tb$DateTime<-gsub("/19 ", "/2019", tb$DateTime)

# convert to POSIXlt
tb$DateTime<-as.POSIXlt(tb$DateTime, format="%m/%d/%Y %H:%M", tz = 'UTC')


# convert each tip to depth in cm & get daily rainfall totals
tb_day_sum<-tb%>%mutate(rain = 0.01*2.54, day = as.Date(DateTime))%>%group_by(day)%>%dplyr::summarize(rain = sum(rain))
tb<-tb%>%mutate(rain = 0.01*2.54)
tb<-tb[,-2]
tb_hour<- aggregate(tb['rain'], list(hour=cut(as.POSIXct(tb$DateTime), "hour")), sum)
tb_hour$hour<-as.POSIXct(tb_hour$hour, format="%Y-%m-%d %H:%M:%S", tz = 'UTC')

# write daily rainfall and list of events to csv to clean up
# write.csv(tb_day_sum, "temp_rainfall.csv")
# temp<-Total[complete.cases(Total$Volume), c(2,3)]
# write.csv(temp, "temp_tile.csv")

# read back in: ev = event rain
ev<-read.csv(file = ".temp_rainfall.csv")

# clean up
ev<-ev[complete.cases(ev$even.total),]%>%mutate(Event = as.Date(day, format = "%m/%d/%Y"))
ev<-ev[,c(4,3)]
colnames(ev)<-c('Event', "Rain")


# merge even rainfall with Total
Total<-merge(ev, Total, by.x = "Event", all = T)


# calculate runoff ratios
Total$RR<-Total$Volume/(Total$Rain*10)

# add Antecedent rainfall and other event metrics:
# time to peak minutes
# max 

comb_temp<-data.frame()

i<-50
for (i in seq(1,length(Flow_save))){
  
  # event and site name
  CNL<-nchar(names(Flow_save)[i])
  Site<-paste(substring(names(Flow_save)[i], c(1,6), c(5,6)), collapse="") 
  Event<-as.Date(paste(substring(names(Flow_save)[i], c(17,CNL-4), c(CNL-5,CNL-4)), collapse=""), format = "%m%d%Y")
  if (Site == "AHSSUB"){DA<-14.16}
  if (Site == "DCSSUB"){DA<-8}
  if (Site == "DCNSUB"){DA<-4.85}
  
  # event start
  es<-Flow_save[[i]]$DateTime[1]
  
  # event end
  en<-Flow_save[[i]]$DateTime[length(Flow_save[[i]]$DateTime)]
  
  # event rain
  er <- tb[tb$DateTime >= (es-86400) & tb$DateTime <= en,]
  
  # event rain with hourly data
  erh<-tb_hour[tb_hour$hour >= (es-86400) & tb_hour$hour <= en,]
  
  # metrics
  mhri<-max(erh$rain, na.rm = T)*10*24 # max rainfall intensity in mm/day
  mhri<-ifelse(mhri>0, mhri, NA)
  prior_rain_24H<-sum(er$rain[er$DateTime<es])*10 # prior rain in mm
  response_time<-as.numeric(es - er$DateTime[1], units = 'hours') # reponse time
  response_time<-ifelse(response_time > 0, response_time, NA) # only keep postive response times
  df_7d <- tb[tb$DateTime >= (es-604800) & tb$DateTime <= en,] # 7 day rainfall df
  prior_rain_7D <- sum(df_7d$rain[df_7d$DateTime<es]) *10 # 7 D rainfall in mm
  df_30d <- tb[tb$DateTime >= (es-2592000) & tb$DateTime <= en,] # 30 day rainfall df
  prior_rain_30D <- sum(df_30d$rain[df_30d$DateTime<es]) *10 #30 D rainfall in mm
  peak_flow_rate<-max(Flow_save[[i]]$Flow, na.rm = T) *(1/1000)*(1/DA)*(1/10000)*(1000)*(60*60*24) # peak flow rate in mm/day
  ttpm<-min(as.numeric(Flow_save[[i]]$DateTime[which(Flow_save[[i]]$Flow == max(Flow_save[[i]]$Flow))])-as.numeric(es))/60   # time to peak minutes
  
  
  temp<-data.frame(Event, Site, prior_rain_24H, prior_rain_7D, prior_rain_30D, mhri, response_time, ttpm, peak_flow_rate)
  
  comb_temp<-rbind(comb_temp, temp)
  
}

comb_temp<-comb_temp%>%arrange(Event, Site)

# merge
Total<-merge(comb_temp, Total, all = T)

# add seasons
Total<-Total%>%
  mutate(Season4 = getSeason_hydrologic(Event), WaterYearDay = get_waterYearDay(Event))

bottles<-bottles%>% 
  rowwise() %>% 
  mutate(Day.since.P = get_P_app_day(Event, Site), Day.since.Manure = get_manure_app_day(Event, Site), Season4 = getSeason_hydrologic(Event), WY = lfstat::water_year(Event, origin = 'usgs'))%>%
  ungroup()%>%
  dplyr::select(-'PP')


# save worksapce dataframes for rmarkdown
dfs<-Filter(function(x) is.list(get(x)) , ls())

dfs

dfs<-dfs[c(51,6)] # Total and bottles

save(list=dfs, file="work2.RData")

#### Annual load Table ####

# Adding in missed events by hands

# First thing is to reduce Total dataframe columns
# i.e. remove unnecessary data and restructer dataframe
# after previous section

Total<-Total%>%dplyr::select(c(Event,Rain,Site,Volume,TP.Load,TP.Conc,SRP.Load,SRP.Conc,Day.since.P,Day.since.Manure,Season,WY,RR))

# Need to adjust for events that were estimated by hand:
# 1) Add in Events that Don estimated that I have no hydrographs OR if I did have a hydrograph but he estimated more, replace those values
# 2) for the second case in part 1) above, remove estimates of the events FWMC  
# 3) Estimate loads for missed events at the dead creek sites using values from the other


# 1) & 2) Events loads from Dons estimations:
# note: this also includes three from summer of 2020 that were set equal to value at DCN
Site<-"AHSSUB"
Event<-as.Date(c("11/2/2018","12/2/2018","1/24/2019","2/5/2019","6/20/2019","10/31/2019","11/19/2019","12/17/2019","1/6/2020","2/26/2020","3/6/2020","3/13/2020", "5/12/2020", "8/4/2020", "9/30/2020"), format = "%m/%d/%Y")
TP.Load<-c(21.7,96.5,84.9, 57.9,170.6,394.4,73.5,11,11.9,45.6,61.6,32.5,35.4,10.7,17.9)
AHSEEL<-data.frame(Site,Event, TP.Load)

# join and create dummy column for finding duplicates
Total<-full_join(Total, AHSEEL)%>%arrange(Site, Event)%>% mutate(site.event = paste0(Site,Event)) 

# get rows to be removed that are duplicaes using loop
# r= row numbers that are duplicates, will be removed after loop
r<-0

for (i in seq(2,length(Total$Site))){
  if (Total$site.event[i] == Total$site.event[i-1]){ # if row is a duplicate, just want to keep the second row, 
    r<-c(r,i-1) # this gives a list of rows to delete
  }
}

Total<-Total[-r,] # remove rows thats data has been appended to row above

Total<-Total[,-length(Total)] # remove site.event dummy col


# add in missing Dead creek events by hand
# did it in excel to make assumptions easier
DCEEL<-read.csv(".Dead Creek Missing Load estimations for R.csv"); colnames(DCEEL)<-colnames(AHSEEL)
DCEEL<-DCEEL%>%mutate(Event = as.Date(Event, format = "%m/%d/%Y"))


Total<-full_join(Total, DCEEL)%>% # join
  arrange(Site, Event)%>% # arrange
  mutate(site.event = paste0(Site,Event)) # create dummy column for removing duplicates

# get rows to be removed that are duplicaes using loop
# r= row numbers that are duplicates, will be removed after loop
r<-0

for (i in seq(2,length(Total$Site))){
  if (Total$site.event[i] == Total$site.event[i-1]){ # if row is a duplicate, want to keep the first row for volumes and conc, second row for loads
    Total$TP.Load[i-1]<-Total$TP.Load[i]
    r<-c(r,i) # this gives a list of rows to delete, i.e. want to delete second row of duplicate group
  }
}

Total<-Total[-r,] # remove rows thats data has been appended to row above

Total<-Total[,-length(Total)] # remove site.event dummy col


## Baseflow

# first create dataframe with daily increments for period of record at each site
# DCS and Dcn this is WY 2020 and 2021
# at AHS this is WY 2019 and 2020
DCS<- data.frame(Event = seq(as.Date("10/01/2019", format = "%m/%d/%Y", tz = 'UTC'), as.Date("09/30/2021", format = "%m/%d/%Y", tz = 'UTC'), "day"))
DCN<- data.frame(Event = seq(as.Date("10/01/2019", format = "%m/%d/%Y", tz = 'UTC'), as.Date("09/30/2021", format = "%m/%d/%Y", tz = 'UTC'), "day"))
AHS<- data.frame(Event = seq(as.Date("10/01/2018", format = "%m/%d/%Y", tz = 'UTC'), as.Date("09/30/2020", format = "%m/%d/%Y", tz = 'UTC'), "day"))

# then merge event load data: Event, Volume, TP.Load, R.R.
DCS<-left_join(DCS, Total[Total$Site == "DCSSUB",c(1,4,5,13)])
DCN<-left_join(DCN, Total[Total$Site == "DCNSUB", c(1,4,5,13)])
AHS<-left_join(AHS, Total[Total$Site == "AHSSUB", c(1,4,5,13)])

# add in baseflow data - baseflow assumed to not occur on days with event loads or no event volumes - note there were dates with volumes but no P data
# assign site-specfic baseflow flow rate
# then do pipe for each site
# note drainge area (all 3 differ) and baseflow P concnetrations (DC are same) are different between the sites 
baseflow_DCS<-0.1*60*60*24 # L/day

DCS<-DCS %>% 
  rowwise() %>% # rowwise is like group_by for each row
  mutate(Type = ifelse(is.na(Volume | TP.Load), "Baseflow","Event"),
         TP.Load = ifelse(is.na(TP.Load), baseflow_DCS*25*(1/1000000)*(1/8), TP.Load)) # if TP loads are na, do basefloe math, if no NA, leave as is
  
baseflow_DCN<-0.25*60*60*24 

DCN<-DCN %>% 
  rowwise() %>% 
  mutate(Type = ifelse(is.na(Volume | TP.Load), "Baseflow","Event"), 
         TP.Load = ifelse(is.na(TP.Load), baseflow_DCN*25*(1/1000000)*(1/4.85), TP.Load))


baseflow_AHS<-0.25*60*60*24 

AHS<-AHS %>% 
  rowwise() %>% 
  mutate(Type = ifelse(is.na(Volume | TP.Load), "Baseflow","Event"), 
         TP.Load = ifelse(is.na(TP.Load), baseflow_AHS*15*(1/1000000)*(1/14.16), TP.Load))

# account for drought in 2020, assume tile not flowing from July 1 2020 to Dec 1 2020, set baseflow
# to zero, events loads stay

noflow<-seq(as.Date("07/01/2020", format = "%m/%d/%Y", tz = 'UTC'), as.Date("12/01/2020", format = "%m/%d/%Y", tz = 'UTC'), "day")

DCS$TP.Load[DCS$Event %in% noflow & DCS$Type == "Baseflow"]<-0
DCN$TP.Load[DCN$Event %in% noflow & DCN$Type == "Baseflow"]<-0
AHS$TP.Load[AHS$Event %in% noflow & AHS$Type == "Baseflow"]<-0


# assign season using function
# assign water year using lfstate package function
# assing site here too

DCS<-DCS%>%mutate(Season = getSeason(Event), WY = lfstat::water_year(Event, origin = 'usgs'), Site = "DCS")
DCN<-DCN%>%mutate(Season = getSeason(Event), WY = lfstat::water_year(Event, origin = 'usgs'), Site = "DCN")
AHS<-AHS%>%mutate(Season = getSeason(Event), WY = lfstat::water_year(Event, origin = 'usgs'), Site = "AHS")


## rbind all sites, get stats, make table 

# rbind, rbs = row bind sites
rbs<-as.data.frame(rbind(DCS,DCN,AHS))

# large pipe: 
rbs<-rbs%>%
  dplyr::mutate_at(5:8, as.factor) %>% # need to make Type, Season, WY, Site factors firsts
  dplyr::group_by(WY, Site, Type, Season, .drop = FALSE) %>% 
  dplyr::summarize(Volume = mean(Volume, na.rm = T), RR = mean(RR, na.rm = T), TP.Load = round(sum(TP.Load), 1)) %>%# mean of volumes, sum of TP.Loads
  arrange(as.numeric(levels(WY))[WY]) %>%
  ungroup()%>%
  pivot_longer(cols = c(Volume, TP.Load,RR), names_to = "Data.Type", values_to = "Value") %>%
  dplyr::group_by(WY, Site, Data.Type) %>%
  do(add_row(., summarise(.,
                          across(where(is.numeric), sum),
                          WY = last(WY),
                          Site = last(Site),
                          Type = "WY TOTAL",
                          Season = NA))) %>%
  dplyr::arrange(as.numeric(as.character(WY)), Site) %>%
  ungroup()%>%
  filter(!if_all(c("Data.Type", "Value"), is.na))%>%
  pivot_wider(names_from = c(WY,Type,Season), values_from = c(Value), values_fn = first) %>%
  ungroup() %>%
  mutate_at(c(vars(contains("WY"))), funs(lead), n = 1) %>% # shift total columns up one, note that need to ungroup first!!!!
  drop_na(Data.Type)%>%
  mutate_at(1, as.character) %>% # for some reason it was changing these to numbers if it sites were factors, no idea
  mutate_all(~ifelse(is.nan(.), NA, .)) %>% # remove NaN
  mutate_all(~ifelse(. == 0, NA, .)) %>% # remove 0's
  mutate_if(is.numeric, round, 2) %>%
  arrange(Site, match(Data.Type, c("TP.Load", "Volume", "RR"))) %>%
  setNames(.,c("Site", "Metric", rep(c("G","NG", "G", "NG", "WY Total"),3))) # keep this as last step in pipe because duplicate names messes thingsup 


## add footnotes

# save as different variable
wrf<-as.data.frame(rbs)

# Events that were missed that needed to be estimated at each site
# create dataframe of number of missing per site, season and water year
tot<-rbind(AHSEEL, DCEEL)
nm<-tot%>%rowwise()%>%mutate(Season = getSeason(Event), WY = lfstat::water_year(Event, origin = 'usgs')) %>% group_by(Site,  WY, Season)%>%dplyr::summarize(N_Events = sum(n()), Total.TP.Load = sum(TP.Load, na.rm = T))

# then looked at 'nm' and made footnotes

# AHS
wrf[1,7]<-paste0(wrf[1,7], footnote_marker_alphabet(1))
wrf[1,12]<-paste0(wrf[1,12], footnote_marker_alphabet(2))

# DCN cells
wrf[4,12]<-paste0(as.character(wrf[4,12]), footnote_marker_alphabet(3))
wrf[4,17]<-paste0(as.character(wrf[4,17]), footnote_marker_alphabet(4))

# DCS cells
wrf[7,12]<-paste0(as.character(wrf[7,12]), footnote_marker_alphabet(5))
wrf[7,17]<-paste0(as.character(wrf[7,17]), footnote_marker_alphabet(6))


# create table
options(knitr.kable.NA = '')

wrf %>%
  kbl(align = "c",escape = F, caption = "Integrate Loadographs") %>%
  kable_classic(html_font = 'Times', font_size = 14, full_width = F) %>%
  add_header_above(c(" " = 2, rep(c("Baseflow"=2, "Event"=2, " " = 1),3))) %>%
  add_header_above(c(" " = 2, "2019" = 4, " " = 1, "2020" = 4, " " = 1, "2021" = 4, " " = 1)) %>%
  # add_header_above(c(" " = 1, "TP Loading (g/ha) by Water Year (Oct-Sep)\nG = Growing Season (May-Sep), NG = Non-growing" = 15))%>%
  collapse_rows(., columns = 1, valign = 'middle')%>%
  row_spec(c(3,6), extra_css = "border-bottom: 1px solid")%>%
  kableExtra::footnote(general = "Sampling errors resulted in the following number of missing events and load estimates:", 
           alphabet = c(
             "Growing: 1 event, 171 g/ha, Non-Growing: 4 events, 261 g/ha",
             "Growing: 3 events, 64 g/ha, Non-Growing: 7 events, 630 g/ha",
             "Non-Growing: 14 events, 2770 g/ha",
             "Growing: 1 event, 2 g/ha",
             "Non-Growing: 14 events, 555 g/ha",
             "Growing: 1 event, 3 g/ha"
             )
          )


# # save worksapce dataframes for rmarkdown
dfs<-Filter(function(x) is.list(get(x)) , ls())

dfs

dfs<-dfs[c(60)] # wrf

save(list=dfs, file="work3.RData")





