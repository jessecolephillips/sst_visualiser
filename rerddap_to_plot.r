# Title: ERDDAP Server to Plot
# Author: Jesse Phillips
# Date: 11/02/2026

# Description:
# RScript that will take inputted spatial and temporal bounds and plot SST using
# ggplot2.
# NB: Will constrain this script to being with, system requirements can get heavy.

# This script will likely remain a way to download and plot sst data in a
# LIGHTWEIGHT manner. Heavier needs should be addressed with the podaac data
# subscriber/downloader python programs.

# PACKAGES ---------------------------------------------------------------
# Ensure required packages are installed:
required_pkgs <- c("dplyr", "ggplot2", "rerddap", "ggspatial")

# If missing, install them:
missing_pkgs <- required_pkgs[
  !sapply(required_pkgs, requireNamespace, quietly = TRUE)
]
if (length(missing_pkgs) > 0L) {
  install.packages(missing_pkgs)
}

# Then quietly load:
invisible(lapply(required_pkgs, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

# Clear environment and run garbage collection
rm(list = ls(all.names = TRUE))
gc()

# SET UP VARS ------------------------------------------------------------
# Create and set all the variables that will be used in downloading and plotting

# Use this function to search for data products on the ERDDAP server:
# ed_search("MUR", url = "https://coastwatch.noaa.gov/erddap/")$info
# The search term goes in the first argument, 'info' returns only the df with
# titles and dataset ids of search hits.

# NB: ONLY USE PROVIDED URL

# Set data set ID of chosen product
dataset_id <- "noaacrwsstDaily"

# Use this function to check connection to ERDDAP servers and info in chosen dataset:
# info(datasetid = dataset_id, url = "https://coastwatch.noaa.gov/erddap/")

# Set temporal and spatial bounds
dates = c('2020-01-01', '2020-01-01') # This bound CAN be just one day if needed (but two values are still required)
lats = c(-38, -24)
longs = c(14, 35)


# DOWNLOAD NC FILE TO STORAGE --------------------------------------------
# This section will download an .nc file with the desired bounds and save it
# to disk.
# NB: The correct pathway MUST be set.

# UNCOMMENT FUNCTION TO RUN

# griddap(
#   datasetx = dataset_id,
#   time = dates,
#   latitude = lats,
#   longitude = longs,
#   url = "https://coastwatch.noaa.gov/erddap/",
#   store = disk(path = "~/sst_visualiser/data"), # Edit pathway here
#   read = FALSE
# )

# LOAD DATA INTO ENVIRONMENT ---------------------------------------------
# This section loads the chosen data directly into the R environment as a
# csv data frame hybrid. Plots can then be made directly from this data.

# NB NB NB: This is where you can overload your memory, make sure you have a
# decent device with decent RAM if you intend on loading a large spatial area
# or long temporal span.

sst_data <- griddap(
  datasetx = dataset_id,
  time = dates,
  latitude = lats,
  longitude = longs,
  fields = "analysed_sst", # Specifiacallty only pulls SST
  fmt = "csv",
  url = "https://coastwatch.noaa.gov/erddap/",
  store = memory()
) %>%
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
  dplyr::rename(
    t = time,
    lat = latitude,
    lon = longitude,
    sst = analysed_sst
  ) %>%
  na.omit()
# The second half of the function uses dplyr functions to edit the resulting data
# frame into something a bit more useable than before.

# PLOT DATA --------------------------------------------------------------
# This is only done if data was loaded into the environment.
# ggplot is used to plot the downloaded SST

# NB: This plotting section is a work in progress - I'm currently still working
# on how to deal with large datasets, or multiple days.

# Save plot as an object
sst_map <- ggplot(sst_data, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = sst)) +
  annotation_borders(col = "black", fill = "cornsilk", linewidth = 0.6) +
  coord_equal(xlim = longs, ylim = lats, expand = 0) +
  # geom_point(
  #   data = coastal_towns,
  #   aes(x = lon, y = lat),
  #   shape = 21,
  #   size = 3,
  #   fill = "#db0e0eff"
  # ) +
  # geom_label(
  #   data = coastal_towns,
  #   aes(x = lon, y = lat, label = names),
  #   nudge_y = 0.5
  # ) +
  # scale_x_continuous(
  #   breaks = seq(15, 35, 5),
  #   labels = c("15°E", "20°E", "25°E", "30°E", "35°E"),
  #   position = "bottom"
  # ) +
  # scale_y_continuous(
  #   breaks = seq(-36, -24, 4),
  #   labels = c("36.0°S", "32.0°S", "28.0°S", "24.0°S"),
  #   position = "right"
  # ) +
  scale_fill_gradient(low = "#03064eff", high = "#35beebff") +
  labs(
    title = "SST around southern Africa",
    #subtitle = sst_data$t,
    x = "",
    y = "",
    fill = "SST (°C)",
    #caption = "Sea Surface Temperature, NOAA Coral Reef Watch Daily Global 5km Satellite SST (CoralTemp), 1985-present, Daily"
  ) +
  theme(
    legend.position = c(0.5, 0.9),
    legend.direction = "horizontal",
    plot.title = element_text(face = "bold")
  )

# FOR TESTING: show plot in viewer pane
sst_map
