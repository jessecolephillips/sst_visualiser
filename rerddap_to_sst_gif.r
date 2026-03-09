# rerddap to GIF Rscript
# 09 March 2026
# Jesse Phillips

# This script will create the LOOP of:
# Accessing an SST product from an ERDDAP server using the rerddap package
# Downloading a days-worth of gridded data around the coast of South Africa
# Creating and saving a plot to a subdirectory of this project
# Repeating this loop, every day, for 40 years - to create a GIF showing ocean temperature and current dynamics

# All plots will be saved to the gif_plots directory (NOT to be uploaded to github)

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

dates <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")

lats = c(-42, -22)
longs = c(12, 38)

# Global SST range (so that colour scale stays the same for every png)
sst_range <- c(7, 32)

# MAIN LOOP --------------------------------------------------------------

for (i in seq_along(dates)) {
  print(dates[i])

  # Get Data
  sst_data <- griddap(
    datasetx = "noaacrwsstDaily", # Dataset title: Sea Surface Temperature, NOAA Coral Reef Watch Daily Global 5km Satellite SST (CoralTemp), 1985-present, Daily
    time = c(as.character(dates[i]), as.character(dates[i])),
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

  # Plot Data
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
    # scale_fill_gradient(low = "#03064eff", high = "#3ddaf2ff") +
    scale_fill_viridis_c(option = "inferno", limits = sst_range) +
    labs(
      title = "SST around southern Africa",
      subtitle = dates[i],
      x = "",
      y = "",
      fill = "SST (°C)",
      # caption = "Sea Surface Temperature, NOAA Coral Reef Watch Daily Global 5km Satellite SST (CoralTemp), 1985-present, Daily"
    ) +
    theme(
      legend.position = c(0.5, 0.9),
      legend.direction = "horizontal",
      plot.title = element_text(face = "bold")
    )

  # Save Plot
  # Format date for naming file
  timestamp <- format(dates[i], "%Y%m%d")
  fname <- paste0("sst_around_sa_", timestamp, ".png")

  ggsave(
    filename = paste0("~/sst_gif_plots/", fname),
    plot = sst_map,
    height = 7,
    width = 8
  )

  # Clean environment before repeating loop (KEEP date, lat and lon)
  rm(sst_data, sst_map, fname, timestamp)
  gc()

  print("Done")

  Sys.sleep(2)
}
