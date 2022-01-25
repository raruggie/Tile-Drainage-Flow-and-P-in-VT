rm(list=ls(all=T))
setwd("C:/School/Data/Data for thesis")

### librarys and options ####
# library(pracma)
# library(scales)
# library(lubridate)
# library(tidyverse)
# library(zoo)
# library(car)
# library(sjPlot)
# library(MASS)
# library(lattice)
# library(knitr)
# library(kableExtra)
# library(lfstat)
# library("PerformanceAnalytics")

# options
options(max.print = 10000)

#### end ####

load("workspaceX.Rdata")

# regressions between tiles
# to help estimate missing loads

ahs<-Total%>%filter(Site == "AHSSUB")
dcs<-Total%>%filter(Site == "DCSSUB")
dcn<-Total%>%filter(Site == "DCNSUB")

#### Volume Regressions ####

## Predicting: at DCN using:

# Volume:
# DCS:
m<-full_join(dcs,dcn)%>%arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(Volume)) & n()==2) %>%arrange(Site, Event)%>%select_if(~ !any(is.na(.)))%>% pivot_wider(names_from = Site,values_from = Volume, )%>%ungroup()%>%dplyr::select(-Event)#%>%mutate_all(., sqrt)
plot(m$DCSSUB,m$DCNSUB, main = "DCN v.s. DCS - Volume");m<-lm(m);abline(m);summary(m)

# Volume:
# AHS:
m<-full_join(dcn,ahs)%>%arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(Volume)) & n()==2) %>%arrange(Site, Event)%>%select_if(~ !any(is.na(.)))%>%pivot_wider(names_from = Site,values_from = Volume, )%>%ungroup()%>%dplyr::select(-Event)
plot(m$AHSSUB, m$DCNSUB, main = "AHS v.s. DCS - Volume");m<-lm(m$DCNSUB~m$AHSSUB);abline(m);summary(m)

# TP.Load:
# DCS:
m<-full_join(dcs,dcn)%>% arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(TP.Load)) & n()==2) %>%arrange(Site, Event)%>%dplyr::select(c(Site, Event, TP.Load))%>%pivot_wider(names_from = Site,values_from = TP.Load,)%>%ungroup()%>%dplyr::select(-Event)
plot(m$DCSSUB, m$DCNSUB);m<-lm(m);abline(m);summary(m)

# TP.Load:
# AHS:
m<-full_join(dcn,ahs)%>% arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(TP.Load)) & n()==2) %>%arrange(Site, Event)%>%dplyr::select(c(Site, Event, TP.Load))%>%pivot_wider(names_from = Site,values_from = TP.Load,)%>%ungroup()%>%dplyr::select(-Event)
m<-m[m$AHSSUB !=m$DCNSUB,] # for this one only need to remove identicals because I used DCN to estimate loads at AHS in summer of 2020
plot(m$AHSSUB, m$DCNSUB);m<-lm(m$DCNSUB~m$AHSSUB);abline(m);summary(m)

# TP.Conc:
# DCS:
m<-full_join(dcs,dcn)%>% arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(TP.Conc)) & n()==2) %>%arrange(Site, Event)%>%dplyr::select(c(Site, Event, TP.Conc))%>%pivot_wider(names_from = Site,values_from = TP.Conc,)%>%ungroup()%>%dplyr::select(-Event)
plot(m$DCSSUB, m$DCNSUB);m<-lm(m);abline(m);summary(m)

# TP.Conc:
# AHS:
m<-full_join(ahs,dcn)%>% arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(TP.Conc)) & n()==2) %>%arrange(Site, Event)%>%dplyr::select(c(Site, Event, TP.Conc))%>%pivot_wider(names_from = Site,values_from = TP.Conc,)%>%ungroup()%>%dplyr::select(-Event)
plot(m$AHSSUB, m$DCNSUB);m<-lm(m$DCNSUB~m$AHSSUB);abline(m);summary(m)

## Predicting: at DCS using:

# Volume:
# DCN:
m<-full_join(dcs,dcn)%>%arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(Volume)) & n()==2) %>%arrange(Site, Event)%>%select_if(~ !any(is.na(.)))%>% pivot_wider(names_from = Site,values_from = Volume, )%>%ungroup()%>%dplyr::select(-Event)#%>%mutate_all(., sqrt)
plot(m$DCNSUB,m$DCSSUB, main = "DCN v.s. DCS - Volume");m<-lm(m$DCSSUB~m$DCNSUB);abline(m);summary(m)

# Volume:
# AHS:
m<-full_join(dcs,ahs)%>%arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(Volume)) & n()==2) %>%arrange(Site, Event)%>%select_if(~ !any(is.na(.)))%>%pivot_wider(names_from = Site,values_from = Volume, )%>%ungroup()%>%dplyr::select(-Event)
plot(m$AHSSUB, m$DCSSUB, main = "AHS v.s. DCS - Volume");m<-lm(m$DCSSUB~m$AHSSUB);abline(m);summary(m)

# TP.Load:
# DCN:
m<-full_join(dcs,dcn)%>% arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(TP.Load)) & n()==2) %>%arrange(Site, Event)%>%dplyr::select(c(Site, Event, TP.Load))%>%pivot_wider(names_from = Site,values_from = TP.Load,)%>%ungroup()%>%dplyr::select(-Event)
plot(m$DCNSUB, m$DCSSUB);m<-lm(m$DCSSUB~m$DCNSUB);abline(m);summary(m)

# TP.Load:
# AHS:
m<-full_join(dcs,ahs)%>% arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(TP.Load)) & n()==2) %>%arrange(Site, Event)%>%dplyr::select(c(Site, Event, TP.Load))%>%pivot_wider(names_from = Site,values_from = TP.Load,)%>%ungroup()%>%dplyr::select(-Event)
plot(m$AHSSUB, m$DCSSUB);m<-lm(m$DCSSUB~m$AHSSUB);abline(m);summary(m)

# TP.Conc:
# DCN:
m<-full_join(dcs,dcn)%>% arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(TP.Conc)) & n()==2) %>%arrange(Site, Event)%>%dplyr::select(c(Site, Event, TP.Conc))%>%pivot_wider(names_from = Site,values_from = TP.Conc,)%>%ungroup()%>%dplyr::select(-Event)
plot(m$DCNSUB, m$DCSSUB);m<-lm(m$DCSSUB~m$DCNSUB);abline(m);summary(m)

# TP.Conc:
# AHS:
m<-full_join(ahs,dcs)%>% arrange(Event, Site)%>%group_by(Event)%>%filter(!any(is.na(TP.Conc)) & n()==2) %>%arrange(Site, Event)%>%dplyr::select(c(Site, Event, TP.Conc))%>%pivot_wider(names_from = Site,values_from = TP.Conc,)%>%ungroup()%>%dplyr::select(-Event)
plot(m$AHSSUB, m$DCSSUB);m<-lm(m$DCSSUB~m$AHSSUB);abline(m);summary(m)


