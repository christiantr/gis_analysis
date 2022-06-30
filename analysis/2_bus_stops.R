library(tidyverse)
library(cowplot)
library(scales)

source("./analysis/0_read.R")






############# 1. Prepare Data: Find nearest stop of all distances

origin.bus <- st_drop_geometry(origin_stop) %>%
  filter(closest == "Bus") %>%
  add_column(leg = "1_first")
destination.bus <- st_drop_geometry(destination_stop) %>%
  filter(closest == "Bus") %>%
  add_column(leg = "2_last")

all.bus <- bind_rows(origin.bus, destination.bus)
all.bus$county <- fct_relevel(all.bus$county, sort)


############# 2. Make Individual Plots
p_distance <- ggplot(all.bus, aes(x = county, y = distance, fill = leg)) +
  geom_boxplot(alpha = 1.0, lwd = 0.4) +
  xlab("") +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  ylab("walking distance") +
  scale_y_continuous(
    breaks = c(0, 0.5, 1, 2, 3, 4, 5, 7.5, 10),
    labels = c("0m", "500m", "1km", "2km", "3km", "4km", "5km", "7.5km", "10km")
  ) +
  coord_flip() +
  scale_fill_discrete(name = "leg", labels = c("first", "last")) +
  theme(legend.position = "bottom")



p_elevation <- ggplot(all.bus, aes(x = county, y = elev_cum, fill = leg)) +
  geom_boxplot(alpha = 1.0, lwd = 0.4) +
  xlab("") +
  ylab("cumulative elevation") +
  scale_y_continuous(
    breaks = c(0, 50, 100, 250, 500, 1000, 1500),
    labels = c("0hm", "50hm", "100hm", "250hm", "500hm", "1000hm", "1500hm")
  ) +
  coord_flip()

p_slope <- ggplot(all.bus, aes(x = county, y = slope_max , fill = leg)) +
  geom_boxplot(alpha = 1.0, lwd = 0.4) +
  xlab("") +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  ylab("maximal slope") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35),
    limits = c(0, 0.36)
  ) +
  coord_flip()


####### 3. Arrange Plots
p <- plot_grid(p_distance + theme(legend.position = "none"),
  p_elevation + theme(legend.position = "none"),
  p_slope + theme(legend.position = "none"),
  get_legend(p_distance),
  ncol = 1, align = "v", axis = "l",
  rel_heights = c(1, 1, 1, 0.15)
)

p
####### 3. Save Plot

ggsave(
  "fig_bus_summary.pdf",
  plot = p,
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 16.5,
  height = 14,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)
ggsave(
  "fig_bus_summary.jpg",
  plot = p,
  device = "jpg",
  path = NULL,
  scale = 1,
  width = 16.5,
  height = 14,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

###### 4. Quantile Values


quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("{{ x }}" := quantile(x, q), "{{ x }}_q" := q)
}

quantiles<-c(0.25,0.5,0.75,0.95, 0.99)

all.bus %>%
  filter(leg == "1_first") %>%
  summarise(quibble(distance, quantiles))
all.bus %>%
  filter(leg == "1_first") %>%
  drop_na(elev_cum) %>%
  summarise(quibble(elev_cum, quantiles))
all.bus %>%
  filter(leg == "1_first") %>%
  drop_na(slope_max ) %>%
  summarise(quibble(slope_max , quantiles))


all.bus %>%
  filter(leg == "2_last") %>%
  summarise(quibble(distance, quantiles))
all.bus %>%
  filter(leg == "2_last") %>%
  drop_na(elev_cum) %>%
  summarise(quibble(elev_cum, quantiles))
all.bus %>%
  filter(leg == "2_last") %>%
  drop_na(slope_max ) %>%
  summarise(quibble(slope_max , quantiles))




all.bus %>%
  filter(leg == "1_first") %>%  filter(county == "Fresach")%>% 
  summarise(quibble(distance, quantiles))
all.bus %>%
  filter(leg == "2_last") %>% 
  filter(county == "Fresach") %>% 
  summarise(quibble(distance, quantiles))
