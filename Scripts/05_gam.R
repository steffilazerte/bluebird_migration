#' ---
#' title: "GAM Analysis"
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

#' R Code to run and save this report:
#+ eval = FALSE
# rmarkdown::render(input = "Scripts/05_gam.R",
#                   output_dir = 'Results/',
#                   output_file = paste0('05_gam_', Sys.Date(), '.html'),
#                   envir = new.env())
#'
#'
#+ echo = FALSE
# Setup -------------------------------------------------------------------
#' # Setup
#+ message = FALSE

library(tidyverse)
library(sf)
library(mgcv)
library(patchwork) # For combining figures
library(here)
library(SDMTools)

#+ echo = FALSE
# Load Data -------------------------------------------------------------------
#' ## Load Data

#' Load hex polygons and bird counts (could load bird hexes, but takes much longer to run)
birds <- read_rds(here("Data/Datasets/birds_02_hex_sum.rds"))
hex <- read_rds(here("Data/Datasets/hex_res7.rds"))
americas <- read_rds(here("Data/Datasets/americas.rds"))
map_limits <- read_rds(here("Data/Datasets/species_bbox.rds"))

#' `map_limits` created in 03_initial_data_ebird.R

#+ echo = FALSE
# Weighted lat/lon  ---------------------------------------------------------------
#' # Weighted lat/lon (centroids etc.)
#'
#' - ~ L72 hb-migration.r
#' - Get daily average lat/lon weighted by detections and effort (i.e. checklists)
#'   (This will be used in GAM smoothing model)
#'
#' - For each species, on each day, calculate average location
#' - Average location calculated as weighted mean of hex cell centroids
#' - Weighted by number observation (checklists with birds) / number checklists

birds_mean_loc <- birds %>%
  # Omit hex cells with no checklists at all
  filter(!is.na(observation_date)) %>%
  # For each species, on each day, calculate weighted location
  group_by(species, observation_date, yday, year, month, week) %>%
  summarize(center_lon = SDMTools::wt.mean(lon, total_obs/total_checklists),
            center_lat = SDMTools::wt.mean(lat, total_obs/total_checklists),
            sdlon = SDMTools::wt.sd(lon, total_obs/total_checklists),
            sdlat = SDMTools::wt.sd(lat, total_obs/total_checklists),
            numcells = n(),
            numobs = sum(total_obs),
            numlists = sum(total_checklists),
            center_lon_lists = SDMTools::wt.mean(lon, total_checklists),
            center_lat_lists = SDMTools::wt.mean(lat, total_checklists)) %>%
  arrange(species, year, yday) %>%
  ungroup()

hex_species <- birds %>%
  group_by(species, hexID) %>%
  summarize(present = sum(total_obs)) %>%
  filter(present > 0)

#+ echo = FALSE
# GAM  ---------------------------------------------------------------
#' # GAM
#'
#' Get smoothed predicted line of daily location for each species and each year
#'
#' Nest the data for easy looping (creates sub-datasets)
birds_nest <- birds_mean_loc %>%
  nest(data = c(-species, -year)) %>%
  mutate(predicted = NA)

birds_nest

for(i in 1:nrow(birds_nest)) {

  d <- birds_nest$data[[i]]

  lon_gam <- gam(center_lon ~ s(yday, k = 40), gamma = 1.5, data = d)
  lat_gam <- gam(center_lat ~ s(yday, k = 40), gamma = 1.5, data = d)

  lon_gam_lists <- gam(center_lon_lists ~ s(yday, k = 40), gamma = 1.5, data = d)
  lat_gam_lists <- gam(center_lat_lists ~ s(yday, k = 40), gamma = 1.5, data = d)

  predicted <- birds_nest$data[[i]]

  lon_pred <- predict(lon_gam, newdata = predicted, type = "response", se.fit = TRUE)
  lat_pred <- predict(lat_gam, newdata = predicted, type = "response", se.fit = TRUE)

  lon_pred_lists <- predict(lon_gam_lists, newdata = predicted, type = "response", se.fit = TRUE)
  lat_pred_lists <- predict(lat_gam_lists, newdata = predicted, type = "response", se.fit = TRUE)

  predicted <- mutate(predicted,
                      pred_lon = as.numeric(lon_pred$fit),
                      pred_lon_se = as.numeric(lon_pred$se.fit),
                      pred_lat = as.numeric(lat_pred$fit),
                      pred_lat_se = as.numeric(lat_pred$se.fit),
                      pred_lon_lists = as.numeric(lon_pred_lists$fit),
                      pred_lat_lists = as.numeric(lat_pred_lists$fit))

  birds_nest$predicted[i] <- list(predicted)
}

birds_predicted <- unnest(birds_nest, cols = predicted)

#+ echo = FALSE
# Plotting ----------------------------------------------------------------

#' # Plots
p <- birds_predicted %>%
  select(-data, -contains("sdl"), -contains("_se"), -numcells, -numobs, -numlists) %>%
  gather(type, value, center_lon, center_lat, pred_lon, pred_lat) %>%
  separate(type, into = c("type", "coord"), sep = "_", remove = TRUE) %>%
  spread(coord, value) %>%
  arrange(species, type, year, yday) %>%
  filter(!is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(americas)) %>%
  group_by(species, type, year, month) %>%
  summarize(n = n(), do_union = FALSE) %>%
  ungroup()


#+ echo = FALSE
# Eastern Bluebird Overall -------------------------------------------------------------
#' ## Eastern Bluebird Overall
p1 <- filter(p, species == "Eastern Bluebird", type == "center")
g1 <- ggplot(data = p1) +
  geom_sf(data = st_crop(americas, map_limits[["Eastern Bluebird"]]), fill = "white") +
  geom_sf(aes(colour = month), size = 1.5) +
  ggtitle(label = "Average Location") +
  scale_color_gradientn(colours = fields::tim.colors(12))

p2 <- filter(p, species == "Eastern Bluebird", type == "pred") %>%
  st_cast("LINESTRING")
g2 <- ggplot(data = p2) +
  geom_sf(data = st_crop(americas, map_limits[["Eastern Bluebird"]]), fill = "white") +
  geom_sf(aes(colour = month), size = 1) +
  ggtitle(label = "GAM predicted location") +
  scale_color_gradientn(colours = fields::tim.colors(12))

#+ fig.width = 16, fig.asp = 0.6
g1 + g2 + plot_layout(guides = "collect") + plot_annotation(title = "Eastern Bluebird")

#+ echo = FALSE
# Eastern Bluebird By year --------------------------------------------------------
#' ## Eastern Bluebird By Year
g1 <- g1 + facet_wrap(~ year, nrow = 1)
g2 <- g2 + facet_wrap(~ year, nrow = 1)

#+ fig.width = 25, fig.asp = 0.3
g1 + g2 + plot_layout(guides = "collect", ncol = 1)


# Mountain Overall -------------------------------------------------------------
#' ## Mountain Bluebird Overall
p1 <- filter(p, species == "Mountain Bluebird", type == "center")
g1 <- ggplot(data = p1) +
  geom_sf(data = st_crop(americas, map_limits[["Mountain Bluebird"]]), fill = "white") +
  geom_sf(aes(colour = month), size = 1.5) +
  ggtitle(label = "Average Location") +
  scale_color_gradientn(colours = fields::tim.colors(12))

p2 <- filter(p, species == "Mountain Bluebird", type == "pred") %>%
  st_cast("LINESTRING")
g2 <- ggplot(data = p2) +
  geom_sf(data = st_crop(americas, map_limits[["Mountain Bluebird"]]), fill = "white") +
  geom_sf(aes(colour = month), size = 1) +
  ggtitle(label = "GAM predicted location") +
  scale_color_gradientn(colours = fields::tim.colors(12))

#+ fig.width = 16, fig.asp = 0.6
g1 + g2 + plot_layout(guides = "collect") + plot_annotation(title = "Mountain Bluebird")

#+ echo = FALSE
# Mountain By year --------------------------------------------------------
#' ## Mountain Bluebird By Year
g1 <- g1 + facet_wrap(~ year, nrow = 1)
g2 <- g2 + facet_wrap(~ year, nrow = 1)

#+ fig.width = 25, fig.asp = 0.3
g1 + g2 + plot_layout(guides = "collect", ncol = 1)

#+ echo = FALSE
# Western Overall -------------------------------------------------------------
#' ## Western Bluebird Overall
p1 <- filter(p, species == "Western Bluebird", type == "center")
g1 <- ggplot(data = p1) +
  geom_sf(data = st_crop(americas, map_limits[["Western Bluebird"]]), fill = "white") +
  geom_sf(aes(colour = month), size = 1.5) +
  ggtitle(label = "Average Location") +
  scale_color_gradientn(colours = fields::tim.colors(12))

p2 <- filter(p, species == "Western Bluebird", type == "pred") %>%
  st_cast("LINESTRING")
g2 <- ggplot(data = p2) +
  geom_sf(data = st_crop(americas, map_limits[["Western Bluebird"]]), fill = "white") +
  geom_sf(aes(colour = month), size = 1) +
  ggtitle(label = "GAM predicted location") +
  scale_color_gradientn(colours = fields::tim.colors(12))

#+ fig.width = 16, fig.asp = 0.6
g1 + g2 + plot_layout(guides = "collect") + plot_annotation(title = "Western Bluebird")

#+ echo = FALSE
# Western By year --------------------------------------------------------
#' ## Western Bluebird By Year
g1 <- g1 + facet_wrap(~ year, nrow = 1)
g2 <- g2 + facet_wrap(~ year, nrow = 1)

#+ fig.width = 25, fig.asp = 0.3
g1 + g2 + plot_layout(guides = "collect", ncol = 1)

# Save Data ---------------------------------------------------------------
birds_predicted %>%
  select(-data) %>%
  write_csv(here("Data/Datasets/birds_03_mean.csv"))

#+ echo = FALSE
# Reproducible ---------------------------------------------------------------
#' # Reproducible

#' ## Session Info
#+ R.options = list(width = 100)
devtools::session_info()
