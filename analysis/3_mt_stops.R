library(tidyverse)
library(tibble)
library(sf)
library(cowplot)
library(scales)

source("./analysis/0_read.R")




#################################################################################

# 2.0  Prepare
############# Find nearest stop of all distances

origin_stop <- st_drop_geometry(origin_stop) %>%
  filter(closest %in% c("Bus", "Mt")) %>%
  add_column(leg = "1_first")

origin_stop_both <- origin_stop %>%
  group_by(srcID) %>%
  slice(which.min(distance))
origin_stop_both$closest <- "BusMT"

origin_stop <- origin_stop %>%
  bind_rows(., origin_stop_both)
origin_stop$closest <- as_factor(origin_stop$closest)

origin_stop$county <- fct_relevel(origin_stop$county, sort)






destination_stop <- st_drop_geometry(destination_stop) %>%
  filter(closest %in% c("Bus", "Mt")) %>%
  add_column(leg = "2_last")
destination_stop_both <- destination_stop %>%
  group_by(dstID) %>%
  slice(which.min(distance))
destination_stop_both$closest <- "BusMT"

destination_stop <- destination_stop %>%
  bind_rows(., destination_stop_both)
destination_stop$closest <- as_factor(destination_stop$closest)
destination_stop$county <- fct_relevel(destination_stop$county, sort)

all<- bind_rows(origin_stop, destination_stop)






############# 2. Make Individual Plots
names <- c(
  `1_first` = "first leg",
  `2_last` = "last leg"
)

######## 2.1 Distance

p_distance <- ggplot(all, aes(x = county, y = distance, fill = closest)) +
  geom_boxplot(alpha = 1.0, lwd = 0.4) +
  facet_wrap(~ leg,   dir = "v", labeller = as_labeller(names))+
  xlab("") +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  ylab("walking distance") +
  scale_y_continuous(
    breaks = c(0,  1, 2, 3, 4, 5, 7.5, 10),
    labels = c("0m",  "1km", "2km", "3km", "4km", "5km", "7.5km", "10km")
  ) +
  coord_flip() +
  theme(legend.position = "bottom")+
  scale_fill_discrete(name = "closest stop", labels = c("bus", "MT", "bus or MT"))

p_distance




######## 2.2 Elevation



p_elevation <- ggplot(all, aes(x = county, y = elev_cum, fill = closest)) +
  geom_boxplot(alpha = 1.0, lwd = 0.4) +
  facet_wrap(~ leg,   dir = "v", labeller = as_labeller(names))+
  xlab("") +
  ylab("cumulative elevation") +
  scale_y_continuous(
    breaks = c(0,  100, 250, 500, 1000, 1500),
    labels = c("0hm",  "100hm", "250hm", "500hm", "1000hm", "1500hm")
  ) +
  coord_flip()+  
  theme(legend.position = "bottom")+
  scale_fill_discrete(name = "closest stop", labels = c("bus", "MT", "bus or MT"))

p_elevation



######### 2.3 Max Slope

p_slope <- ggplot(all, aes(x = county, y = slope_max , fill = closest)) +
  geom_boxplot(alpha = 1.0, lwd = 0.4) +
  xlab("") +
  facet_wrap(~ leg,   dir = "v", labeller = as_labeller(names))+
  
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  ylab("maximal slope") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35),
    limits = c(0, 0.36)
  ) +
  coord_flip()+
  theme(legend.position = "bottom")+
  scale_fill_discrete(name = "closest Stop", labels = c("bus", "MT", "bus or MT"))

p_slope


#########################################################################



####### 3. Arrange Plots
p <- plot_grid(p_distance + theme(legend.position = "none"),
               p_elevation + theme(legend.position = "none"),
               # p_slope + theme(legend.position = "none"),
               get_legend(p_distance),
               ncol = 1, align = "v", axis = "l",
               rel_heights = c(1, 1, 0.15)
)

p
####### 3. Save Plot

ggsave(
  "fig_mt_summary.pdf",
  plot = p,
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 16.5,
  height = 22,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

ggsave(
  "fig_mt_summary.jpg",
  plot = p,
  device = "jpg",
  path = NULL,
  scale = 1,
  width = 16.5,
  height = 18,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

###########


#### MT stop closer than Bus stops -> source
temp_source<-source_stop %>%  
  filter(closest %in% c("Bus", "BusMT")) %>% 
  select(srcID, dstID, distance,closest,county) %>%
  group_by(srcID) %>%arrange(srcID) %>% 
  spread(closest, distance) %>% select(-dstID) %>% group_by(srcID)

source_bus<- temp_source %>%select(-BusMT)%>% drop_na()  
source_busormt <- temp_source%>%select(-Bus)%>% drop_na()
source_diff <- source_bus %>% 
  left_join(source_busormt) %>% 
  mutate(busormtCloserThanBus=BusMT-Bus) %>%
  ungroup()%>%
  add_column(Type="1_Source")

source_diff_nonzero <- source_diff %>% filter(busormtCloserThanBus<(-0.1))


min(source_diff_nonzero$busormtCloserThanBus)
max(source_diff_nonzero$busormtCloserThanBus)
mean(source_diff_nonzero$busormtCloserThanBus)
median(source_diff_nonzero$busormtCloserThanBus)

#### MT stop closer than Bus stops -> destination
temp_destination<-destination_stop %>%  
  filter(closest %in% c("Bus", "BusMT")) %>% 
  select(srcID, dstID, distance,closest,county) %>%
  group_by(dstID) %>%arrange(dstID) %>%  select(-srcID) %>%
  spread(closest, distance) %>% group_by(dstID)

destination_bus<- temp_destination %>%select(-BusMT)
destination_busormt <- temp_destination%>%select(-Bus)
destination_diff <- destination_bus %>% 
  left_join(destination_busormt) %>% 
  mutate(busormtCloserThanBus=BusMT-Bus) %>%
  ungroup()%>%
  add_column(Type="2_Destination")

destination_diff_nonzero <- destination_diff %>% filter(busormtCloserThanBus<0)
destination_diff_nonzero <- destination_diff %>% filter(busormtCloserThanBus<(-0.1))


min(destination_diff_nonzero$busormtCloserThanBus)
max(destination_diff_nonzero$busormtCloserThanBus)
mean(destination_diff_nonzero$busormtCloserThanBus)
median(destination_diff_nonzero$busormtCloserThanBus)



###### 4. Quantile Values


quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("{{ x }}" := quantile(x, q), "{{ x }}_q" := q)
}

quantiles<-c(0.25,0.5,0.75,0.95, 0.99)

all %>%
  filter(leg == "1_first") %>% filter(closest=="BusMT")%>% 
  summarise(quibble(distance, quantiles))
all %>%
  filter(leg == "1_first") %>% filter(closest=="BusMT")%>% 
  drop_na(elev_cum) %>%
  summarise(quibble(elev_cum, quantiles))
all %>%
  filter(leg == "1_first") %>% filter(closest=="BusMT")%>% 
  drop_na(slope_max ) %>%
  summarise(quibble(slope_max , quantiles))


all %>%
  filter(leg == "2_last") %>% filter(closest=="BusMT")%>% 
  summarise(quibble(distance, quantiles))
all %>%
  filter(leg == "2_last") %>% filter(closest=="BusMT")%>% 
  drop_na(elev_cum) %>%
  summarise(quibble(elev_cum, quantiles))
all %>%
  filter(leg == "2_last") %>% filter(closest=="BusMT")%>% 
  drop_na(slope_max ) %>%
  summarise(quibble(slope_max , quantiles))


#####

all %>%
  filter(leg == "1_first") %>% filter(closest=="BusMT")%>% 
  select(distance) %>%max()
all %>%
  filter(leg == "2_last") %>% filter(closest=="BusMT")%>% 
  select(distance) %>%max()
###
View(all %>% group_by(county) %>%filter(closest=="BusMT")%>%
  select(distance) %>%  summarise(quibble(distance, quantiles)))
