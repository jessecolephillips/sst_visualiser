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
required_pkgs <- c("dplyr", "ggplot2", "rerddap")

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

# Dataset title: Sea Surface Temperature, NOAA Coral Reef Watch Daily Global 5km Satellite SST
# (CoralTemp), 1985-present, Daily

# Use this function to check connection to ERDDAP servers and info in chosen dataset:
# info(datasetid = dataset_id, url = "https://coastwatch.noaa.gov/erddap/")

# The dataset seems to be 2 days behind current day (this is important for the daily plotting idea)

# Get system date and move two days back:
date = Sys.Date() - 2

# Set temporal and spatial bounds
dates = c(date, date) # This bound CAN be just one day if needed (but two values are still required)
lats = c(-44, -22)
longs = c(11, 39)


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
  time = as.character(dates),
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
  scale_x_continuous(
    breaks = seq(15, 35, 5),
    labels = c("15°E", "20°E", "25°E", "30°E", "35°E"),
    position = "bottom"
  ) +
  scale_y_continuous(
    breaks = seq(-40, -25, 5),
    labels = c("40°S", "35°S", "30°S", "25°S"),
    position = "right"
  ) +
  scale_fill_gradient(low = "#03064eff", high = "#3ddaf2ff") +
  labs(
    title = "SST around southern Africa",
    subtitle = date,
    x = "",
    y = "",
    fill = "SST (°C)",
    caption = "Sea Surface Temperature, NOAA Coral Reef Watch Daily Global 5km Satellite SST (CoralTemp), 1985-present, Daily"
  ) +
  theme(
    legend.position = c(0.5, 0.9),
    legend.direction = "horizontal",
    plot.title = element_text(face = "bold")
  )

# FOR TESTING: show plot in viewer pane
sst_map


# SAVE PLOT --------------------------------------------------------------

# Format date for naming file
timestamp <- format(date, "%Y%m%d")
fname <- paste0("sst_around_sa_", timestamp, ".png")


ggsave(
  filename = paste0("plots/", fname),
  plot = sst_map,
  height = 7,
  width = 8
)


# FINAL CLEANUP ----------------------------------------------------------
rm(list = ls(all.names = TRUE))
gc()
