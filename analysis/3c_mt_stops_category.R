library(tidyverse)
library(tibble)
library(sf)
library(cowplot)
library(scales)

source("./analysis/0_read.R")

# Compare Distance, Elevation, Slope  by Category of Destination Locations
origin_mt <- st_drop_geometry(origin_stop) %>%
  filter(closest %in% c("Bus", "Mt")) %>%
  group_by(srcID) %>%
  slice(which.min(distance)) %>%
  add_column(leg = "1_first")

destination_mt <- st_drop_geometry(destination_stop) %>%
  filter(closest %in% c("Bus", "Mt")) %>%
  group_by(dstID) %>%
  slice(which.min(distance)) %>%
  add_column(leg = "2_last")






all <-  destination_mt
all<-  all %>% filter(distance<=2)
all$category <- as_factor(all$category)


names <- c(
  `Amenities` = "amenities",
  `Errands` = "errands",
    `Tourism` = "tourism",
  `LeisureSport` = "leisure & sport"
)

### Distance 
p_distance <- ggplot(all, aes(distance, fill = category)) + 
  geom_histogram(alpha = .7) + 
  facet_grid(. ~ category, labeller = as_labeller(names)) +
  background_grid(major = 'y', minor = "x") +
  ylab("count") +
  xlab("walking distancee")+
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(0, 0.5, 1, 1.5, 2, 3, 4, 5, 10),
    labels = c("0m", "500m", "1.5km","1km", "2km", "3km", "4km", "5km", "10km")
  )



### Elevation 
p_elevation <- ggplot(all, aes(elevTotal, fill = category)) + 
  geom_histogram(alpha = .7) + 
  facet_grid(. ~ category, labeller = as_labeller(names)) +
  background_grid(major = 'y', minor = "x") +
  ylab("count") +
  xlab("elevation difference")+
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(25, 50, 75, 100, 150),
    labels = c("25hm", "50hm", "75hm", "100hm", "150hm"),
    guide = guide_axis(angle = 45)
  )

### Slope
p_slope <- ggplot(all, aes(maxSlope, fill = category)) + 
  geom_histogram(alpha = .7) + 
  facet_grid(. ~ category, labeller = as_labeller(names)) +
  background_grid(major = 'y', minor = "x") +
  ylab("count") +
  theme(legend.position = "none")+
  xlab("maximal slope") +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35),
    guide = guide_axis(angle = 45)
  )
    


p <- plot_grid(
  p_distance + theme(legend.position = "none"),
  p_elevation + theme(legend.position = "none"),
  p_slope + theme(legend.position = "none"),
  align = "vh",
  hjust = -1,
  nrow = 3,
  ncol=1
)

p

ggsave(
  "fig_busmt_categories.pdf",
  plot = p,
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 16.5,
  height = NA,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

ggsave(
  "fig_busmt_categories.jpg",
  plot = p,
  device = "jpg",
  path = NULL,
  scale = 1,
  width = 16.5,
  height = NA,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)




