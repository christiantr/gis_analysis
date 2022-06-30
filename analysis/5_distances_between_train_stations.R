library(tidyverse)
library(tibble)
library(sf)
library(osrm)

source("./analysis/0_read.R")

### Analyze Driving Distance and Times between Train Station along Corridor.

train_stops<-stops %>%filter(type=="Train")

ferndorf<- train_stops[1,]
paternion<- train_stops[2,]
paternion_feistritz<- train_stops[3,]
weissenstein<- train_stops[4,]
puch<- train_stops[5,]


distances <- osrmTable(loc=train_stops)

distances$durations

shortest<- osrmRoute(src = ferndorf, dst = paternion, returnclass = "sf", overview = "full", osrm.profile="car")
longest<- osrmRoute(src = ferndorf, dst = puch, returnclass = "sf", overview = "full", osrm.profile="car")
osrmRoute(src = paternion_feistritz, dst = weissenstein, returnclass = "sf", overview = "full", osrm.profile="car")
