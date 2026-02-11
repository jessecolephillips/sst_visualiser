library(rerddap)
library(dplyr)
library(ggplot2)
library(ggspatial)


# search for data sets
rerddap::ed_search(
  "MUR",
  url = "https://coastwatch.noaa.gov/erddap/"
)$info

# The information for the targetted NOAA data
rerddap::info(
  datasetid = "noaacrwsstDaily",
  url = "https://coastwatch.noaa.gov/erddap/"
)

# SO, rerrdap DOES work, but you MUST use the correct URLs, or things crash.
# Or, if you try and request too much data at once, things crash as well.

# okay, lets try and download some data

?griddap


# Store to disk
griddap(
  "noaacrwsstDaily",
  time = c('2020-01-01', '2020-01-02'),
  latitude = c(-40, -35),
  longitude = c(15, 21),
  url = "https://coastwatch.noaa.gov/erddap/",
  store = disk(path = "~/sst_visualiser/data"),
  read = FALSE
)

# Okay, lets try to save some downloaded data straight to the environment
sst_data <- griddap(
  "noaacrwsstDaily",
  time = c('2020-01-01', '2020-01-01'),
  latitude = c(-38, -24),
  longitude = c(14, 35),
  fields = "analysed_sst",
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


# co-ords of nb coastal towns to add to plot
coastal_towns <- data.frame(
  names = c("CT", "QB", "DB"),
  lon = c(18.4241, 25.6000, 31.0276),
  lat = c(-33.9249, -33.9581, -29.8579)
)


# PLot
plt1 <- ggplot(sst_data, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = sst)) +
  annotation_borders(col = "black", fill = "cornsilk", linewidth = 0.6) +
  coord_equal(xlim = c(14, 35), ylim = c(-38, -24), expand = 0) +
  geom_point(
    data = coastal_towns,
    aes(x = lon, y = lat),
    shape = 21,
    size = 3,
    fill = "#db0e0eff"
  ) +
  geom_label(
    data = coastal_towns,
    aes(x = lon, y = lat, label = names),
    nudge_y = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(15, 35, 5),
    labels = c("15°E", "20°E", "25°E", "30°E", "35°E"),
    position = "bottom"
  ) +
  scale_y_continuous(
    breaks = seq(-36, -24, 4),
    labels = c("36.0°S", "32.0°S", "28.0°S", "24.0°S"),
    position = "right"
  ) +
  scale_fill_gradient(low = "#03064eff", high = "#35beebff") +
  labs(
    title = "SST around southern Africa",
    subtitle = sst_data$t,
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

plt1

# save plot
ggsave(plot = plt1, "plots/test_map.png", height = 6, width = 9)
