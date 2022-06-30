library(tidyverse)
library(tibble)
library(sf)
library(cowplot)

source("./analysis/0_read.R")

### Analyze difference between closest train and bus

#1.1 Source - to Stop
destination_stop <- destination_stop %>% mutate(avgMetersBtwPoints = (distance * 1000 * 2 / lengths(geom)), .after = "distance")
origin_stop <- origin_stop %>% mutate(avgMetersBtwPoints = (distance * 1000 * 2 / lengths(geom)), .after = "distance")

temp_origin<-st_drop_geometry(origin_stop) %>%  
  filter(closest %in% c("Train", "Bus")) %>% 
  select(srcID, dstID, distance,closest,county) %>%
  group_by(srcID) %>%arrange(srcID) %>% 
  spread(closest, distance) %>% select(-dstID) %>% group_by(srcID)

origin_train<- temp_origin %>%select(-Bus)%>% drop_na()  
origin_bus <- temp_origin%>%select(-Train)%>% drop_na()
origin_diff <- origin_train %>% 
  left_join(origin_bus) %>% 
  mutate(busCloserThanTrain=Train-Bus) %>%
  ungroup()%>%
  add_column(leg="1_first")


# Get Numbers
Train_closer<- origin_diff %>% filter(busCloserThanTrain<0) %>% count() %>% pull() 
Bus_closer<- origin_diff %>% filter(busCloserThanTrain>=0) %>% count() %>% pull() 
summary(origin_diff %>% filter(busCloserThanTrain<0))
Train_closer/ (Train_closer+ Bus_closer)



#1.2 Destination - to Stop

temp_destination<-st_drop_geometry(destination_stop) %>%  
  filter(closest %in% c("Train", "Bus")) %>% 
  select(srcID, dstID,name, distance,closest,county,category) %>%
  group_by(dstID) %>% 
  spread(closest, distance) %>% select(-srcID) %>% group_by(dstID) %>%arrange(dstID)

destination_train<- temp_destination %>%select(-Bus)%>% drop_na()  
destination_bus <- temp_destination%>%select(-Train)%>% drop_na()
destination_diff <- destination_train %>% left_join(destination_bus) %>% 
  mutate(busCloserThanTrain=Train-Bus) %>%
  ungroup()%>%
  add_column(leg="2_last")


# Get Numbers
Train_closer<- destination_diff %>% filter(busCloserThanTrain<0) %>% count() %>% pull() 
Bus_closer<- destination_diff %>% filter(busCloserThanTrain>=0) %>% count() %>% pull() 
summary( destination_diff %>% filter(busCloserThanTrain<0))
Train_closer/ (Train_closer+ Bus_closer)


### 1.3 Illustrate in joint plot

all_diff <-  bind_rows(origin_diff, destination_diff)

p1 <- ggplot(all_diff, aes(x = county, y = busCloserThanTrain, fill=leg)) +
  geom_boxplot(alpha=0.6) +
  labs(title = "", fill = "Type of Stop") +
  geom_hline(yintercept = 0, size = 0.5, color = "darkgreen") +
  xlab("") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylab("difference in walking distance \n between closest train stop and closest bus stop") +
  scale_y_continuous(
    breaks = c(-0.5, 0, 0.5,  1, 2, 3, 4, 5, 10),
    labels = c("-500m","0m", "500m", "1km", "2km", "3km", "4km", "5km", "10km")
  )+
  scale_fill_discrete(name = "Leg", labels = c("source to stop", "stop to destination"))

p2 <- ggplot(all_diff, aes(y = busCloserThanTrain, fill=leg)) +
  geom_histogram(alpha=0.6, position="identity", aes(x = ..density..)) +
  labs(title = "", fill = "Type of Stop") +
  geom_hline(yintercept = 0, size = 0.5, color = "darkgreen") +
  ylab("") +
  # xlab("") + 
  scale_y_continuous(
    breaks = c(-0.5, 0, 0.5,  1, 2, 3, 4, 5, 10),
    labels = c("-500m", "0m", "500m", "1km", "2km", "3km", "4km", "5km", "10km")
  )+
  scale_fill_discrete(name = "leg", labels = c("first", "last"))


legend_b <- get_legend(
  p2 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

prow <- plot_grid(
  p1 + theme(legend.position="none"),
  p2 + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 1
)



p<- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))

p

ggsave(
  "fig_diff_bus_and_train.pdf",
  plot = p,
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 16.5,
  height = 13,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

ggsave(
  "fig_diff_bus_and_train.jpg",
  plot = p,
  device = "jpg",
  path = NULL,
  scale = 1,
  width = 16.5,
  height = 13,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

