# clear global environment and set working directory
rm(list=ls(all=T))
# set you working directory that contains files used in this script
setwd("C:/RAR-Uvm/Data/Data for thesis")

#### code goals ####

# process event load data from single file of dead creek data
# then perform paired watershed analysis


#### librarys and options ####
library(tidyverse)


# options
options(max.print = 10000)


#### load data and clean up ####

DC<-read.csv(file = ".Paired Dead Creek Data.csv")
colnames(DC)<-c("Date", colnames(DC)[2:9]) # rename first column because coming in messy from csv
DC$Site<-as.factor(DC$Site) # change to factor so can easily remove the dash from the site name
levels(DC$Site)<-c("DCNSUB", "DCNSUR", "DCSSUB", "DCSSUR")
DC<-DC%>% dplyr::mutate_at(3:9, as.numeric)%>%# convert character columns to numeric. they get loaded as character if there is text in any cells of that column. Note the warning message which results because cells that are text go to NA, which is fin
      mutate(Volume = Runoff..m3.ha. *(1000/10000)) # volume in mm


# break up into paired surface and paired subsurface (i.e. tile drain)
Tile<-DC[grepl("SUB", DC$Site),]
Surf<-DC[grepl("SUR", DC$Site),]

#### Test to see if calibration phase can end ####

# step 1 - are regressions significant between all variables of interest (i.e. volume, Total P (TP) and soluble reactive P (SRP) loads and TP and SRP flow weighted mean concentrations)?
# step 2 - are there enough events [to detect reasonable change in treatment]?
# step 3 - are residual errors smaller than expected BMP effect (not sure how this differes from step 2)

# just using Tile data and volumes as an example for methods, then can code the other variables of interests as well as the surface watersheds

# Create dataframe of paired volume data at each site 
m<-Tile%>%
  group_by(Date)%>%
  filter(!any(is.na(Volume)))%>% 
  dplyr::select(c(Date,Site,Volume))%>% # remove unwanted columns
  mutate(Volume = log(Volume))%>%
  pivot_wider(names_from = Site,values_from = Volume) %>%# get responsive and predictor variable data into columns
  as.data.frame(.)

m %>%
  ggplot(aes(x = DCSSUB, y = DCNSUB))+
  geom_point() + geom_smooth(method = "lm")
  
summary(lm(DCNSUB~DCSSUB, data = m))




# first remove rows with NA in desired column, then only keep duplicated for tile/surface
# then run linear regression

## surface

sr<-DC[DC$Site == 'DCSSUR' | DC$Site == "DCNSUR",] # subset only surface data

#### runoff volumes ####

s_runoff<-sr%>%filter(complete.cases(Runoff..m3.ha.))%>% group_by(Date) %>% filter( n() > 1 )
plot(log(s_runoff$Runoff..m3.ha.[s_runoff$Site == "DCSSUR"]), log(s_runoff$Runoff..m3.ha.[s_runoff$Site == "DCNSUR"])) # plot 

m_runoff<-lm(log(s_runoff$Runoff..m3.ha.[s_runoff$Site == "DCNSUR"])~log(s_runoff$Runoff..m3.ha.[s_runoff$Site == "DCSSUR"])) # create model object
summary(m_runoff) # summary of model, check p-value of slope
abline(m_runoff) # add regression line to plot
hist(log(s_runoff$Runoff..m3.ha.[s_runoff$Site == "DCNSUR"])) # check histograms for normality
hist(log(s_runoff$Runoff..m3.ha.[s_runoff$Site == "DCSSUR"]))

#### end surface volumes runoff ####

# since runoff volume is not significant, others are not tested yet


## tile

t<-DC[DC$Site == "DCSSUB" | DC$Site == "DCNSUB",] # subset tile data

#### runoff volumes ####

t_runoff<-t%>%filter(complete.cases(Runoff..m3.ha.))%>% group_by(Date) %>% filter( n() > 1 )
plot(log(t_runoff$Runoff..m3.ha.[t_runoff$Site == "DCSSUB"]), log(t_runoff$Runoff..m3.ha.[t_runoff$Site == "DCNSUB"]))

m_runoff<-lm(log(t_runoff$Runoff..m3.ha.[t_runoff$Site == "DCNSUB"])~log(t_runoff$Runoff..m3.ha.[t_runoff$Site == "DCSSUB"]))
summary(m_runoff)
abline(m_runoff)
hist(log(t_runoff$Runoff..m3.ha.[t_runoff$Site == "DCNSUB"]))
hist(log(t_runoff$Runoff..m3.ha.[t_runoff$Site == "DCSSUB"]))

#### end runoff ####

#### TP concentration ####
t_conc<-t%>%filter(complete.cases(TP.conc))%>% group_by(Date) %>% filter( n() > 1 )
plot(log(t_conc$TP.conc[t_conc$Site == "DCSSUB"]), log(t_conc$TP.conc[t_conc$Site == "DCNSUB"]))

m_conc<-lm(log(t_conc$TP.conc[t_conc$Site == "DCNSUB"])~log(t_conc$TP.conc[t_conc$Site == "DCSSUB"]))
summary(m_conc)
abline(m_conc)
hist(log(t_conc$TP.conc[t_conc$Site == "DCNSUB"]))
hist(log(t_conc$TP.conc[t_conc$Site == "DCSSUB"]))

#### end TP conc ####

#### TP loads ####
t_load<-t%>%filter(complete.cases(TP.load))%>% group_by(Date) %>% filter( n() > 1 )
plot(log(t_load$TP.load[t_load$Site == "DCSSUB"]), log(t_load$TP.load[t_load$Site == "DCNSUB"]))

m_load<-lm(log(t_load$TP.load[t_load$Site == "DCNSUB"])~log(t_load$TP.load[t_load$Site == "DCSSUB"]))
summary(m_load)
abline(m_load)
hist(log(t_load$TP.load[t_load$Site == "DCNSUB"]))
hist(log(t_load$TP.load[t_load$Site == "DCSSUB"]))

#### end TP load ####

#### SRP concentration ####
t_SRPconc<-t%>%filter(complete.cases(SRP.conc))%>% group_by(Date) %>% filter( n() > 1 )
plot(log(t_SRPconc$SRP.conc[t_SRPconc$Site == "DCSSUB"]), log(t_SRPconc$SRP.conc[t_SRPconc$Site == "DCNSUB"]))

t_SRPconc<-t_SRPconc[t_SRPconc$SRP.conc>0,]


m_SRPconc<-lm(log(t_SRPconc$SRP.conc[t_SRPconc$Site == "DCNSUB"])~log(t_SRPconc$SRP.conc[t_SRPconc$Site == "DCSSUB"]))
summary(m_SRPconc)
abline(m_SRPconc)
hist(log(t_SRPconc$SRP.conc[t_SRPconc$Site == "DCNSUB"]))
hist(log(t_SRPconc$SRP.conc[t_SRPconc$Site == "DCSSUB"]))

#### end SRP conc ####

#### SRP load ####
t_SRPload<-t%>%filter(complete.cases(SRP.load))%>% group_by(Date) %>% filter( n() > 1 )
plot(log(t_SRPload$SRP.load[t_SRPload$Site == "DCSSUB"]), log(t_SRPload$SRP.load[t_SRPload$Site == "DCNSUB"]))

t_SRPload<-t_SRPload[t_SRPload$SRP.load>0,]


m_SRPload<-lm(log(t_SRPload$SRP.load[t_SRPload$Site == "DCNSUB"])~log(t_SRPload$SRP.load[t_SRPload$Site == "DCSSUB"]))
summary(m_SRPload)
abline(m_SRPload)
hist(log(t_SRPload$SRP.load[t_SRPload$Site == "DCNSUB"]))
hist(log(t_SRPload$SRP.load[t_SRPload$Site == "DCSSUB"]))

#### end SRP load ####
