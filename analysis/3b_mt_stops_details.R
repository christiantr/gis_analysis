library(tidyverse)
library(tibble)
library(sf)
library(leaflet)
library(cowplot)
library(scales)

source("./analysis/0_read.R")

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





all <- bind_rows(origin_mt, destination_mt)
all<-  all %>% filter(distance<=2)





names <- c(
  `1_first` = "first leg",
  `2_last` = "last leg"
)

### Plot LM Distance vs Elevation
p_lm_distance_elevation <- ggplot(all, aes(distance, elev_cum, color = leg)) + 
  geom_point(alpha = .7) + 
  stat_smooth(method = "lm", formula =y ~ x-1) +
  facet_grid(. ~ leg, labeller = as_labeller(names)) +
  background_grid(major = 'y', minor = "x") +
  xlab("cumulative elevation") +
  ylab("walking distance")+
  panel_border() +
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(0, 0.5, 1, 1.5, 2, 3, 4, 5, 10),
    labels = c("0m", "500m", "1.5km","1km", "2km", "3km", "4km", "5km", "10km")
  )+
scale_y_continuous(
  breaks = c(25, 50, 75, 100, 150,200),
  labels = c("25hm", "50hm", "75hm", "100hm", "150hm", "200hm")
) 
 
  


### Density Plot Elevation
p_density_elevation <- ggplot(all, aes(elev_cum, fill = leg)) +
  geom_density(alpha = .7) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(legend.justification = "top")+
  xlab("cumulative elevation") +
  scale_x_continuous(
    breaks = c(25, 50, 75, 100, 150,200),
    labels = c("25hm", "50hm", "75hm", "100hm", "150hm", "200hm"),
    guide = guide_axis(angle = 45)
  ) +
  scale_fill_discrete(name = "leg", labels = c("first", "last"))

p_density_elevation_nl <- p_density_elevation + theme(legend.position = "none")


### Density Plot Slope
p_density_slope <- ggplot(all, aes(slope_max, fill = leg)) +
  geom_density(alpha = .7) + 
  xlab("maximal slope") +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35),
    limits=c(0.0, 0.4),
    guide = guide_axis(angle = 45)
  )+
  theme(legend.position = "none")

# legend
legend <- get_legend(p_density_elevation)


# align all plots vertically
plots <- align_plots(p_lm_distance_elevation, p_density_elevation_nl, p_density_slope, align = 'v', axis = 'l')

bottom_row <- plot_grid(
  plots[[2]], plots[[3]], legend,
  rel_widths = c(1, 1, .3),
  nrow = 1
)

p <- plot_grid(plots[[1]], bottom_row, 
               ncol = 1)



p

ggsave(
  "fig_busmt_distance_vs_elevation.pdf",
  plot = p,
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 16.5,
  height = 12,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)
ggsave(
  "fig_busmt_distance_vs_elevation.jpg",
  plot = p,
  device = "jpg",
  path = NULL,
  scale = 1,
  width = 16.5,
  height = 12,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

####Lm model

data<-all %>% filter(leg=="1_first")%>%select(distance, elev_cum) %>% mutate(distance=distance*1000)

lm<-lm(elev_cum ~distance -1, data = data)
summary(lm)

#residual standard error
summary(lm)$sigma

