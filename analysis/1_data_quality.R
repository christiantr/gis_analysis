library(tidyverse)
library(tibble)
library(sf)

source("./analysis/0_read.R")

############  Check Distance between points ############ 
destination_stop <- destination_stop %>% mutate(avgMetersBtwPoints = (distance * 1000 * 2 / lengths(geom)), .after = "distance")
origin_stop <- origin_stop %>% mutate(avgMetersBtwPoints = (distance * 1000 * 2 / lengths(geom)), .after = "distance")

summary(destination_stop$avgMetersBtwPoints)
summary(origin_stop$avgMetersBtwPoints)

avgMetersBtwPoints <-  as_tibble(c(destination_stop$avgMetersBtwPoints, origin_stop$avgMetersBtwPoints))
summary(avgMetersBtwPoints)

ggplot(data = avgMetersBtwPoints, aes(x=value)) +
  geom_histogram() +
  xlab("Meters between points on linestring (average)")

ggplot(data = avgMetersBtwPoints, aes(x=value)) +
  geom_boxplot() +
  xlab("Meters between points on linestring (average)")





###### 4. Quantile Values


quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("{{ x }}" := quantile(x, q), "{{ x }}_q" := q)
}

quantiles<-c(0.25,0.5,0.75,0.95, 0.99)

dest<-destination_stop %>%
filter(closest %in% c("Bus","MT")) %>%  
drop_na(slope_max) %>%
st_drop_geometry()  %>%select(slope_max)

org<-
origin_stop  %>% 
filter(closest %in% c("Bus","Mt")) %>%
drop_na(slope_max) %>%
st_drop_geometry()  %>%select(slope_max)

slopes<- bind_rows(dest,org)

slopes %>%summarise(quibble(slope_max, quantiles))


