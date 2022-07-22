#' ---
#' title: "Publication Figures"
#' author: "Steffi LaZerte"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: cosmo
#'     toc: true
#'     toc_float:
#'       collapsed: false
#' ---
#'
#' <!--- This style block is to tweak the formatting of the data tables in the report --->
#' <style>
#'   table {
#'     display: block;
#'     overflow: auto;
#'   }
#'
#' </style>
#'
#' R Code to run and save this report:
#+ eval = FALSE
# rmarkdown::render(input = "Scripts/09_figures.R",
#                   output_dir = 'Results/',
#                   output_file = paste0('09_figures_', Sys.Date(), '.html'),
#                   envir = new.env())
#'
#'
#+ echo = FALSE
# Setup -------------------------------------------------------------------
#' # Setup
#+ message = FALSE
library(tidyverse)
library(sf)
library(patchwork) # For combining figures
library(gridExtra) # Also for combining figures
library(here)
library(rnaturalearth)
library(ggplot2)
library(ggpubr)
require(dplyr)

#+ echo = FALSE
# Load Data ---------------------------------------------------------------
#' ## Load data
birds_predicted <- read_csv(here("Data/Datasets/birds_03_mean.csv"))
migration_details <- read_csv(here("Data/Datasets/migration_details.csv"))
migration <- read_csv(here("Data/Datasets/birds_04_migration.csv"))

americas <- read_rds(here("Data/Datasets/americas.rds"))
map_limits <- read_rds(here("Data/Datasets/species_bbox.rds"))


#+ echo = FALSE
# Figures ---------------------------------------------------------------
#' # Figures

#+ echo = FALSE
## Figure 1 - Combo Map ---------------------------------
#' ## Figure 1 - Combo Map

#' ### Summarize for plotting
plot_dates <- migration_details %>%
  select(species, year, spring_begin_yday, spring_end_yday, fall_begin_yday,
         fall_end_yday) %>%
  pivot_longer(names_to = "event", values_to = "yday", cols = c(-species, -year)) %>%
  mutate(event = factor(
    event,
    levels = c("spring_begin_yday", "spring_end_yday",
               "fall_begin_yday", "fall_end_yday"),
    labels = c("Spring start",
               "Spring end",
               "Fall start",
               "Fall end")))

p <- birds_predicted %>%
  select(-contains("sdl"), -contains("_se"), -numcells, -numobs, -numlists) %>%
  gather(type, value, center_lon, center_lat, pred_lon, pred_lat) %>%
  separate(type, into = c("type", "coord"), sep = "_", remove = TRUE) %>%
  spread(coord, value) %>%
  arrange(species, type, year, yday) %>%
  filter(!is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(americas)) %>%
  group_by(species, type, year, month) %>%
  summarize(n = n(), do_union = FALSE) %>%
  ungroup() %>%
  rename(Month = month)


#' ### Bounding boxes and geo types

map_limits$`Eastern Bluebird`$xmin <- map_limits$`Eastern Bluebird`$xmin + 50000

eb <- st_bbox(c(xmin = -93, xmax = -70, ymin = 25, ymax = 45), crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_transform(st_crs(p)) %>%
  st_bbox()

mb <- st_bbox(c(xmin = -122, xmax = -75, ymin = 20, ymax = 65), crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_transform(st_crs(p)) %>%
  st_bbox()

wb <- st_bbox(c(xmin = -124, xmax = -102, ymin = 27, ymax = 46), crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_transform(st_crs(p)) %>%
  st_bbox()


p$geometry[p$type == "pred"] <- st_cast(p$geometry[p$type == "pred"], "LINESTRING")


#' ### Panels
#'
g1 <- filter(p, species == "Eastern Bluebird") %>%
  ggplot() +
  geom_sf(data = st_crop(americas, eb), fill = "white") +
  geom_sf(aes(colour = Month, size = type)) +
  scale_color_gradientn(colours = fields::tim.colors(12)) +
  facet_wrap(~ type) +
  scale_size_manual(values = c("center" = 1.5, "pred" = 0.75), guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(name = "Eastern Bluebirds", expand = c(0, 0))

g2 <- filter(p, species == "Mountain Bluebird") %>%
  ggplot() +
  geom_sf(data = st_crop(americas, mb), fill = "white") +
  geom_sf(aes(colour = Month, size = type)) +
  scale_color_gradientn(colours = fields::tim.colors(12)) +
  facet_wrap(~ type)+
  scale_size_manual(values = c("center" = 1.5, "pred" = 0.75), guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(name = "Mountain Bluebirds", expand = c(0, 0))

g3 <- filter(p, species == "Western Bluebird") %>%
  ggplot() +
  geom_sf(data = st_crop(americas, wb), fill = "white") +
  geom_sf(aes(colour = Month, size = type)) +
  scale_color_gradientn(colours = fields::tim.colors(12)) +
  facet_wrap(~ type)+
  scale_size_manual(values = c("center" = 1.5, "pred" = 0.75), guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(name = "Western Bluebirds", expand = c(0, 0))

g4 <- ggplot(data = filter(migration, species == "Eastern Bluebird"),
             aes(x = yday, y = pred_lat, group = year)) +
  geom_line() +
  geom_vline(data = filter(plot_dates, species == "Eastern Bluebird"),
             aes(xintercept = yday, colour = event), size = 0.25) +
  scale_colour_viridis_d(name = "Events", end = 0.8) +
  scale_fill_manual(name = "", values = "black") +
  scale_y_continuous(labels = ~paste0(., "\u00B0N")) +
  labs(x = "") +
  guides(colour = guide_legend(title = NULL, nrow = 2, override.aes = list(size = 6)))

g5 <- ggplot(data = filter(migration, species == "Mountain Bluebird"),
             aes(x = yday, y = pred_lat, group = year)) +
  geom_line() +
  geom_vline(data = filter(plot_dates, species == "Mountain Bluebird"),
             aes(xintercept = yday, colour = event), size = 0.25) +
  scale_colour_viridis_d(name = "Events", end = 0.8) +
  scale_fill_manual(name = "", values = "black") +
  scale_y_continuous(labels = ~paste0(., "\u00B0N")) +
  labs(x = "") +
  guides(colour = guide_legend(title = NULL, nrow = 2, override.aes = list(size = 6)))

g6 <- ggplot(data = filter(migration, species == "Western Bluebird"),
             aes(x = yday, y = pred_lat, group = year)) +
  geom_line() +
  geom_vline(data = filter(plot_dates, species == "Western Bluebird"),
             aes(xintercept = yday, colour = event), size = 0.25) +
  scale_colour_viridis_d(name = "Events", end = 0.8) +
  scale_fill_manual(name = "", values = "black") +
  scale_y_continuous(labels = ~paste0(., "\u00B0N")) +
  labs(x = "Day of Year") +
  guides(colour = guide_legend(title = NULL, nrow = 2, override.aes = list(size = 6)))

#' ### Combine
g_map <- g1 / g2 / g3  &
  theme(strip.background = element_blank(), strip.text = element_blank(),
        legend.title = element_text(vjust = 0.7),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)))

g_lat <- g4 / g5 / g6 &
  theme(axis.title.y = element_blank())

g <- g_map + g_lat +
  plot_layout(guides = "collect",
              design = c(area(1, 1, 1, 3),
                         area(2, 1, 2, 3),
                         area(3, 1, 3, 3),
                         area(1, 4, 3, 5))) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom", legend.margin = margin(0, 70, 0, 115),
        legend.box.spacing = unit(5, "pt"),
        plot.margin = unit(c(2, 2, 2, 2), "pt"))


ggsave("Results/Figures/figure1_combo.png", plot = g, dpi = 600, width = 9, height = 9)




#+ echo = FALSE
## Figure 2 - Migration Speed ---------------------------------
#' ## Figure 2 - Migration Speed

par(mfrow=c(1,2))
boxplot(speed_spring~species, ylab = "population-level spring migration speed (km/day)", xlab ="")
boxplot(speed_fall~species, ylab = "population-level spring migration speed (km/day)", xlab ="")

#+ echo = FALSE
## Figure 3 - Migration Longitude  ---------------------------------
#' ## Figure 3 - Migration Longitude

###Fall

ggplot(aes(year, fall_median_lon, color=species, group = species), data=biglon) + geom_point() +
      geom_smooth(method="lm",se=FALSE,colour="black",data=subset(biglon, species=="Eastern Bluebird"))+
      geom_smooth(method="lm",se=FALSE,colour="black",data=subset(biglon, species=="Western Bluebird")) + scale_color_manual(breaks = c("Eastern Bluebird", "Mountain Bluebird", "Western Bluebird"), values=c("orange", "blue", "black"))+

      #geom_point(aes(year, fall_median_lon), data=migration.Eastern.Bluebird) +
      #geom_point(aes(year, fall_median_lon), data=migration.Western.Bluebird) +
      #geom_smooth(method = "lm", se = FALSE, data = migration.Western.Bluebird) +
      #geom_smooth(method = "lm", se = FALSE, data = migration.Eastern.Bluebird) + scale_color_grey(start=0.8, end=0.2) +
      ylab ("Fall median longitude") + xlab ("Year") + theme(legend.position = "none")+
      theme_bw()+
      theme(strip.background = element_blank(), strip.text.x = element_blank(), strip.text.y = element_blank())+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      facet_wrap(~ax, scales="free_y", ncol=1) +scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      theme(axis.title.y.right = element_blank(),                # hide right axis title
            axis.text.y.right = element_blank(),                 # hide right axis labels
            #axis.ticks.y = element_blank(),                      # left/right axis ticks
            axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
            panel.spacing = unit(0, "mm"),                       # remove spacing between facets
            strip.background = element_rect(size = 0.5),
            panel.background = element_rect(colour = "black"))+
      theme(legend.position = "none")+
      theme(text = element_text(size=10))+
      theme(axis.text.x=element_text(colour="black"))+
      theme(axis.text.y=element_text(colour="black"))+
      theme(text = element_text(size=20))


###Spring

ggplot(aes(year, spring_median_lon, color=species, group = species), data=biglon) + geom_point() +
      geom_smooth(method="lm",se=FALSE,colour="black",data=subset(biglon, species=="Eastern Bluebird"))+
      geom_smooth(method="lm",se=FALSE,colour="black",data=subset(biglon, species=="Western Bluebird")) + scale_color_manual(breaks = c("Eastern Bluebird", "Mountain Bluebird", "Western Bluebird"), values=c("orange", "blue", "black"))+

      #geom_point(aes(year, fall_median_lon), data=migration.Eastern.Bluebird) +
      #geom_point(aes(year, fall_median_lon), data=migration.Western.Bluebird) +
      #geom_smooth(method = "lm", se = FALSE, data = migration.Western.Bluebird) +
      #geom_smooth(method = "lm", se = FALSE, data = migration.Eastern.Bluebird) + scale_color_grey(start=0.8, end=0.2) +
      ylab ("Spring median longitude") + xlab ("Year") + theme(legend.position = "none")+
      theme_bw()+
      theme(strip.background = element_blank(), strip.text.x = element_blank(), strip.text.y = element_blank())+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      facet_wrap(~ax, scales="free_y", ncol=1) +scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      theme(axis.title.y.right = element_blank(),                # hide right axis title
            axis.text.y.right = element_blank(),                 # hide right axis labels
            #axis.ticks.y = element_blank(),                      # left/right axis ticks
            axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
            panel.spacing = unit(0, "mm"),                       # remove spacing between facets
            strip.background = element_rect(size = 0.5),
            panel.background = element_rect(colour = "black"))+
      theme(legend.position = "none")+
      theme(text = element_text(size=10))+
      theme(axis.text.x=element_text(colour="black"))+
      theme(axis.text.y=element_text(colour="black"))+
      theme(text = element_text(size=20))



###Breeding

ggplot(aes(year, breeding_median_lon, color=species, group = species), data=biglon) + geom_point() +
      geom_smooth(method="lm",se=FALSE,colour="black",data=subset(biglon, species=="Eastern Bluebird"))+
      geom_smooth(method="lm",se=FALSE,colour="black",data=subset(biglon, species=="Western Bluebird")) + scale_color_manual(breaks = c("Eastern Bluebird", "Mountain Bluebird", "Western Bluebird"), values=c("orange", "blue", "black"))+

      #geom_point(aes(year, fall_median_lon), data=migration.Eastern.Bluebird) +
      #geom_point(aes(year, fall_median_lon), data=migration.Western.Bluebird) +
      #geom_smooth(method = "lm", se = FALSE, data = migration.Western.Bluebird) +
      #geom_smooth(method = "lm", se = FALSE, data = migration.Eastern.Bluebird) + scale_color_grey(start=0.8, end=0.2) +
      ylab ("Breeding median longitude") + xlab ("Year") + theme(legend.position = "none")+
      theme_bw()+
      theme(strip.background = element_blank(), strip.text.x = element_blank(), strip.text.y = element_blank())+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      facet_wrap(~ax, scales="free_y", ncol=1) +scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      theme(axis.title.y.right = element_blank(),                # hide right axis title
            axis.text.y.right = element_blank(),                 # hide right axis labels
            #axis.ticks.y = element_blank(),                      # left/right axis ticks
            axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
            panel.spacing = unit(0, "mm"),                       # remove spacing between facets
            strip.background = element_rect(size = 0.5),
            panel.background = element_rect(colour = "black"))+
      theme(legend.position = "none")+
      theme(text = element_text(size=10))+
      theme(axis.text.x=element_text(colour="black"))+
      theme(axis.text.y=element_text(colour="black"))+
      theme(text = element_text(size=20))

