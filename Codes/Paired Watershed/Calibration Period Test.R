# clear global environment and set working directory
rm(list=ls(all=T))

# set working directory to source file which contains csv used
setwd("C:/RAR-Uvm/Data/Data for thesis")


#### librarys and options ####
library(tidyverse)
library(purrr)

# options
options(max.print = 10000)


#### load data and clean up ####

DC<-read.csv(file = ".Paired Dead Creek Data.csv")
colnames(DC)<-c("Event", colnames(DC)[2:9]) # rename first column because coming in messy from csv
DC$Site<-as.factor(DC$Site) # change to factor so can easily remove the dash from the site name
levels(DC$Site)<-c("DCNSUB", "DCNSUR", "DCSSUB", "DCSSUR")
DC<-DC%>% dplyr::mutate_at(3:9, as.numeric)%>%# convert character columns to numeric. they get loaded as character if there is text in any cells of that column. Note the warning message which results because cells that are text go to NA, which is fin
      mutate(Volume = Runoff..m3.ha. *(1000/10000))%>%# volume in mm
      select(-c(Runoff..m3.ha.,Event.Volume..m3., Drainage.Area..ha.)) # remove unwanted columns
  


# break up into paired surface and paired subsurface (i.e. tile drain)
Tile<-DC[grepl("SUB", DC$Site),]
Surf<-DC[grepl("SUR", DC$Site),]

#### Test to see if calibration phase can end ####

# step 1 - are regressions significant between all variables of interest (i.e. volume, Total P (TP) and soluble reactive P (SRP) loads and TP and SRP flow weighted mean concentrations)?
# step 2 - are there enough events [to detect reasonable change in treatment]?
# step 3 - are residual errors smaller than expected BMP effect (not sure how this differes from step 2)

### Step 1: 


## Tile 

# get means of each metric for control watershed, which is predictor variable X in experiment:

Tile_means<-data.frame(
  map(names(Tile)[-(1:2)], ~ Tile %>%
     # // select the columns of interest along with looped column names
     select(Event, Site, all_of(.x))%>%
     # // grouped by Event and remove groups based on the NA in the looped column
     group_by(Event)%>%
     filter(!any(is.na(.data[[.x]])))%>%
     ungroup()%>%
     # // convert the column looped to its `log`
     mutate(!! .x := log10(.data[[.x]]))%>% 
     # // reshape from long to wide
     pivot_wider(names_from = Site,values_from = all_of(.x)) %>% 
     summarize(m = mean(DCSSUB, na.rm = T))%>%
    rename_at("m", ~ .x) 
  )
)%>%pivot_longer(everything(),values_to = "Mean")

# filter data and create linear model 
Tile.1 <- as.data.frame(map_df(map(names(Tile)[-(1:2)], ~ Tile %>%
             # // select the columns of interest along with looped column names
             select(Event, Site, all_of(.x))%>%
             # // grouped by Event and remove groups based on the NA in the looped column
             group_by(Event)%>%
             filter(!any(is.na(.data[[.x]])))%>%
             ungroup()%>%
             # // convert the column looped to its `log`
             mutate(!! .x := log10(.data[[.x]]))%>% 
             # // reshape from long to wide
             pivot_wider(names_from = Site,values_from = all_of(.x)) %>% 
             # // build the linear model 
             lm(DCNSUB~DCSSUB, data = .)), 
             # // format list into tibble of two variables, pval and MSE
             ~ {v1 <- summary(.x)
             tibble(n = nobs(.x),pval = v1$coefficients[,4][2], MSE = v1$sigma^2)}
))%>% mutate_if(is.numeric, round,4)

# add metrics names
Tile.1<-Tile.1%>%
  mutate(Metric = names(Tile[-(1:2)]), Mean = Tile_means$Mean)%>% # add metric names and means
  select(Metric,Mean,MSE,n,pval) # reorder



## Surface

# get means of each metric for control watershed, which is predictor variable X in experiment:

Surf_means<-data.frame(
  map(names(Tile)[-(1:2)], ~ Surf %>%
        # // select the columns of interest along with looped column names
        select(Event, Site, all_of(.x))%>%
        # // grouped by Event and remove groups based on the NA in the looped column
        group_by(Event)%>%
        filter(!any(is.na(.data[[.x]])))%>%
        ungroup()%>%
        # // convert the column looped to its `log`
        mutate(!! .x := log10(.data[[.x]]))%>% 
        # // reshape from long to wide
        pivot_wider(names_from = Site,values_from = all_of(.x)) %>% 
        summarize(m = mean(DCSSUR, na.rm = T))%>%
        rename_at("m", ~ .x) 
  )
)%>%pivot_longer(everything(),values_to = "Mean")


Surf.1 <- as.data.frame(map_df(map(names(Surf)[-(1:2)], ~ Surf %>%
                                   # // select the columns of interest along with looped column names
                                   select(Event, Site, all_of(.x))%>%
                                   # // grouped by Event and remove groups based on the NA in the looped column
                                   group_by(Event)%>%
                                   filter(!any(is.na(.data[[.x]])))%>%
                                   ungroup()%>%
                                   # // convert the column looped to its `log`
                                   mutate(!! .x := log10(.data[[.x]]))%>% 
                                   # // reshape from long to wide
                                   pivot_wider(names_from = Site,values_from = all_of(.x)) %>%   
                                   # // build the linear model 
                                   lm(DCNSUR~DCSSUR, data = .)), 
                             # // format list into tibble of two variables, pval and MSE
                             ~ {v1 <- summary(.x)
                             tibble(n = nobs(.x),pval = v1$coefficients[,4][2], MSE = v1$sigma^2)}
))%>% mutate_if(is.numeric, round,3)

# add metrics names
Surf.1<-Surf.1%>%
  mutate(Metric = names(Surf[-(1:2)]), Mean = Surf_means$Mean)%>% # add metric names and means
  select(Metric,Mean,MSE,n,pval) # reorder





## LMM

# Tile

map(names(Tile)[-(1:2)], ~ Tile %>%
           # // select the columns of interest along with looped column names
           select(Event, Site, all_of(.x))%>%
           # // grouped by Event and remove groups based on the NA in the looped column
           group_by(Event)%>%
           filter(!any(is.na(.data[[.x]])))%>%
           ungroup()%>%
           # // change Event to numbers instead of full data to shorten ggplot legend
           mutate(Event = as.character(cumsum(c(1,diff(duplicated(Event)) != 1 ))))%>%
           # // ggplot
           ggplot(.,mapping = aes(x = Site, y = .data[[.x]]))+
           geom_point(aes(color = Event))+
           geom_line(aes(x = Site, y = .data[[.x]], group = Event))+
           ggtitle(paste("Tile", .x))
)%>%ggpubr::ggarrange(plotlist = ., nocl = 2, common.legend = T, legend = 'right')


# Surface

map(names(Tile)[-(1:2)], ~ Surf %>%
      # // select the columns of interest along with looped column names
      select(Event, Site, all_of(.x))%>%
      # // grouped by Event and remove groups based on the NA in the looped column
      group_by(Event)%>%
      filter(!any(is.na(.data[[.x]])))%>%
      ungroup()%>%
      # // change Event to numbers instead of full data to shorten ggplot legend
      mutate(Event = as.character(cumsum(c(1,diff(duplicated(Event)) != 1 ))))%>%
      # // ggplot
      ggplot(.,mapping = aes(x = Site, y = .data[[.x]]))+
      geom_point(aes(color = Event))+
      geom_line(aes(x = Site, y = .data[[.x]], group = Event))+
      ggtitle(paste("Surface", .x))
)%>%ggpubr::ggarrange(plotlist = ., nocl = 2, common.legend = T, legend = 'right')





