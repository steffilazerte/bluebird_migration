#' ---
#' title: "Migration Routes"
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
# rmarkdown::render(input = "Scripts/08_migration_routes.R",
#                   output_dir = 'Results/',
#                   output_file = paste0('08_migration_routes_', Sys.Date(), '.html'),
#                   envir = new.env())

#'
#+ echo = FALSE
# Setup -------------------------------------------------------------------
#' # Setup
#+ message = FALSE
library(tidyverse)
library(here)
library(patchwork)

#+ echo = FALSE
# Load Data -------------------------------------------------------------------
#' ## Load Data

#' Load hex polygons and birds counts (could load swift hexes, but takes much
#' longer to run)

migration <- read_csv(here("Data/Datasets/birds_04_migration.csv"), guess_max = 20000) %>%
  mutate(season = factor(season, levels = c("non-breeding", "spring", "breeding", "fall")))
details <- read_csv(here("Data/Datasets/migration_details.csv")) %>%
  select(-contains("_lon")) # Remove old values if rerunning

#+ echo = FALSE
# Summarize Longitudes ----------------------------------------------------
#' # Summarize Longitudes
#'
#' - Extract min, median, mean, mid-point, and maximum longitude during spring
#'   and fall migrations
#' - These longitudes represent the minimum longitude recorded on a given day,
#'   calculated from the longitudes predicted by the GAM
#'
#' > I suggest using the **mid-point** or **median** as they are less sensitive
#' > to extremes.
#' > However, because these are calculated from daily longitudes, the **median**
#' > can be influenced by the speed of migration whereas the **mid-point** cannot
#'

longitudes <- migration %>%
  filter(!is.na(center_lon)) %>%
  group_by(species, year, season) %>%
  summarize(max_lon = max(pred_lon),
            min_lon = min(pred_lon),
            median_lon = median(pred_lon),
            mean_lon = mean(pred_lon),
            midpoint_lon = ((max_lon - min_lon) / 2) + min_lon)

details <- longitudes %>%
  pivot_longer(cols = c(max_lon, min_lon, median_lon, mean_lon, midpoint_lon),
               names_to = "lon_type", values_to = "lon") %>%
  mutate(lon_type = paste0(season, "_", lon_type)) %>%
  select(-season) %>%
  pivot_wider(names_from = lon_type, values_from = lon) %>%
  right_join(details, ., by = c("species", "year"))


#' ## Overall
#'
#' - The figures show the change in Longtitude of spring and fall migrations
#'   over the years
#' - The bars are the min/max longitudes of a specific year and seasonal migration
#' - In the first figure, the points are median longitudes
#' - In the second figure, the points are the longitudes at the mid-point
#'
#' **Note:** Interesting that it almost looks like an Western shift in Eastern
#'   bluebirds and an Eastern shift in Western bluebirds. Even the turn-around
#'   in the last four years or so seem to mirror each other...
#'
#'
#+ fig.asp = 0.75

ggplot(data = longitudes,
       aes(x = median_lon, y = year, colour = season)) +
  theme_bw() +
  geom_pointrange(aes(xmin = min_lon, xmax = max_lon)) +
  geom_point() +
  scale_colour_manual(name = "Season", values = c("non-breeding" = "black",
                                                  "spring" = "cadetblue",
                                                  "breeding" = "black",
                                                  "fall" = "orange")) +
  facet_grid(season ~ species, scales = "free_x") +
  labs(title = "Min - Median - Max Longitude", x = "Median Longitude +/- range")

ggplot(data = longitudes,
       aes(x = midpoint_lon, y = year, colour = season)) +
  theme_bw() +
  geom_pointrange(aes(xmin = min_lon, xmax = max_lon)) +
  geom_point() +
  scale_colour_manual(name = "Season", values = c("non-breeding" = "black",
                                                  "spring" = "cadetblue",
                                                  "breeding" = "black",
                                                  "fall" = "orange")) +
  facet_grid(season ~ species, scales = "free_x") +
  labs(title = "Min - Midpoint - Max Longitude", x = "Midpoint Longitude +/- range")


#' ## Plot routes with Lon/Lat
#'
#' - Visualize the five different longitude measurements
#' - These are some busy plots that show more specifically the context of the
#'   longitude measurements for each migration season for each species.

longitudes_plot <- pivot_longer(longitudes,
                                cols = contains("lon"),
                                names_to = "lon_type",
                                values_to = "lon")

#' ### Western Blubird routes
#'
#' > - Really wonky looking, but perhaps more related to small scale
#' > - 2009 has no fall because start/end of fall is the same point
#' > - 2012 has similar problem
#'
#+ fig.asp = 2
ggplot(data = filter(migration, species == "Western Bluebird", !is.na(center_lon)),
             aes(x = pred_lon, y = pred_lat, group = interaction(season, year),
                 colour = season, size = season)) +
  geom_vline(data = filter(longitudes_plot, species == "Western Bluebird",
                           lon_type == "midpoint_lon"),
             aes(xintercept = lon, colour = season)) +
  labs(title = "Migration routes - Western Bluebird",
       subtitle = "Vertical lines = Mid-point Longitude") +
  theme_bw() +
  geom_point() +
  scale_fill_manual(values = c("fall" = "orange", "spring" = "cadetblue")) +
  facet_grid(year ~ .) +
  scale_colour_manual(name = "Season",
                      values = c("non-breeding" = "black",
                                 "spring" = "cadetblue",
                                 "breeding" = "black",
                                 "fall" = "orange")) +
  scale_size_manual(name = "Season",
                    values = c("non-breeding" = 0.5, "spring" = 2,
                               "breeding" = 0.5, "fall" = 2))

#' ### Mountain Blubird routes
#+ fig.asp = 2
ggplot(data = filter(migration, species == "Mountain Bluebird", !is.na(center_lon)),
       aes(x = pred_lon, y = pred_lat, group = interaction(season, year),
           colour = season, size = season)) +
  geom_vline(data = filter(longitudes_plot, species == "Mountain Bluebird",
                           lon_type == "midpoint_lon"),
             aes(xintercept = lon, colour = season)) +
  labs(title = "Migration routes - Mountain Bluebird",
       subtitle = "Vertical lines = Mid-point Longitude") +
  theme_bw() +
  geom_point() +
  scale_fill_manual(values = c("fall" = "orange", "spring" = "cadetblue")) +
  facet_grid(year ~ .) +
  scale_colour_manual(name = "Season",
                      values = c("non-breeding" = "black",
                                 "spring" = "cadetblue",
                                 "breeding" = "black",
                                 "fall" = "orange")) +
  scale_size_manual(name = "Season",
                    values = c("non-breeding" = 0.5, "spring" = 2,
                               "breeding" = 0.5, "fall" = 2))

#' ### Eastern Blubird routes
#'
#' > Looks like the breeding and overwintering centroids are moving
#'
#+ fig.asp = 2
ggplot(data = filter(migration, species == "Eastern Bluebird", !is.na(center_lon)),
       aes(x = pred_lon, y = pred_lat, group = interaction(season, year),
           colour = season, size = season)) +
  geom_vline(data = filter(longitudes_plot, species == "Eastern Bluebird",
                           lon_type == "midpoint_lon"),
             aes(xintercept = lon, colour = season)) +
  labs(title = "Migration routes - Eastern Bluebird",
       subtitle = "Vertical lines = Mid-point Longitude") +
  theme_bw() +
  geom_point() +
  scale_fill_manual(values = c("fall" = "orange", "spring" = "cadetblue")) +
  facet_grid(year ~ .) +
  scale_colour_manual(name = "Season",
                      values = c("non-breeding" = "black", "spring" = "cadetblue",
                                 "breeding" = "black", "fall" = "orange")) +
  scale_size_manual(name = "Season",
                    values = c("non-breeding" = 0.5, "spring" = 2,
                               "breeding" = 0.5, "fall" = 2))


#' ## Plot routes with error

#' ### Bluebird routes
g <- list()
for(s in c("Western Bluebird", "Mountain Bluebird", "Eastern Bluebird")) {
  g[[s]] <- ggplot(data = filter(migration, species == s, !is.na(center_lon)),
                   aes(x = pred_lon, y = pred_lat, group = interaction(season, year))) +
    theme_bw() +
    geom_rect(aes(xmin = pred_lon - 1.96 * pred_lon_se,
                  ymin = pred_lat - 1.96 * pred_lat_se,
                  xmax = pred_lon + 1.96 * pred_lon_se,
                  ymax = pred_lat + 1.96 * pred_lat_se,
                  fill = season, alpha = season),
              colour = NA) +
    geom_path() +
    scale_alpha_manual(values = c("non-breeding" = 0.005,
                                  "spring" = 0.1,
                                  "breeding" = 0.005,
                                  "fall" = 0.1)) +
    scale_fill_manual(values = c("non-breeding" = "black",
                                 "fall" = "orange",
                                 "breeding" = "black",
                                 "spring" = "cadetblue"),
                      guide = guide_legend(override.aes = list(alpha = 1))) +
    facet_grid( ~ season) +
    labs(title = s)
}

#+ fig.width = 8, fig.asp = 1.6
wrap_plots(g, ncol = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Migration routes and error (95%)")



#' **Note**: Error isn't as strong as it should be, due to the fact that
#' observations are averaged by hex/date before entering the GAM...



#+ echo = FALSE
# Save longitude to details ----------------------------------------------------------
#' ## Save longitude to details
#'
write_csv(details, here("Data/Datasets/migration_details.csv"))

#+ echo = FALSE
# Reproducible ---------------------------------------------------------------
#' # Reproducible

#' ## Session Info
#+ R.options = list(width = 100)
devtools::session_info()
