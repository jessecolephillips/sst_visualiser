# Download and plot west coast sst data
# 13/02/2026

library(rerddap)
library(dplyr)
library(ggplot2)
library(ggpubr)


# dataset_id <- "noaacwLEOACSPOSSTL3SCDaily"

# dataset title:
# Sea-Surface Temperature, NOAA ACSPO Daily Global 0.02° Gridded Super-collated SST
# and Thermal Fronts Reanalysis, 2012-present, Daily (L3S-LEO Kelvin)

info("noaacwLEOACSPOSSTL3SCDaily", url = "https://coastwatch.noaa.gov/erddap/")


sst_data <- griddap(
  "noaacwLEOACSPOSSTL3SCDaily",
  time = c('2020-01-01', '2020-01-01'),
  latitude = c(-35, -29),
  longitude = c(16, 20),
  fields = "sea_surface_temperature",
  fmt = "csv",
  url = "https://coastwatch.noaa.gov/erddap/",
  store = memory()
) %>%
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
  dplyr::rename(
    t = time,
    lat = latitude,
    lon = longitude,
    sst = sea_surface_temperature
  ) %>%
  na.omit()


p1 <- ggplot(sst_data, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = sst)) +
  annotation_borders(col = "black", fill = "cornsilk", linewidth = 0.6) +
  coord_equal(xlim = c(16, 20), ylim = c(-35, -29), expand = 0) +
  scale_fill_gradient(low = "#03064eff", high = "#35beebff") +
  labs(x = "", y = "", fill = "SST (°C)") +
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )


# Wind data (speed and direction)

info("noaacwBlendedWindsDaily", url = "https://coastwatch.noaa.gov/erddap/")

wind_data <- griddap(
  "noaacwBlendedWindsDaily",
  time = c('2020-01-01', '2020-01-01'),
  latitude = c(-35, -29),
  longitude = c(16, 20),
  fields = "all",
  fmt = "csv",
  url = "https://coastwatch.noaa.gov/erddap/",
  store = memory()
) %>%
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
  dplyr::rename(
    t = time,
    lat = latitude,
    lon = longitude
  ) %>%
  na.omit()

# Convert wind direction components for plotting purposes:

# Scaling factor to control arrow length
scale_factor <- 0.03

# The multiplication by scale_factor rescales the physical wind vectors (in m s⁻¹)
# into map units (degrees). Without this step, arrows would dwarf the coordinate grid.
# The appropriate scaling depends on the spatial extent of the plot; there is no
# universal constant.

wind_plot_df <- wind_data %>%
  mutate(
    lon_end = lon + u_wind * scale_factor,
    lat_end = lat + v_wind * scale_factor,
    wind_speed = sqrt(u_wind^2 + v_wind^2)
  )

p2 <- ggplot(wind_plot_df, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = wind_speed)) +
  annotation_borders(col = "black", fill = "cornsilk", linewidth = 0.6) +
  coord_equal(xlim = c(16, 20), ylim = c(-35, -29), expand = 0) +
  geom_segment(
    aes(xend = lon_end, yend = lat_end),
    arrow = arrow(length = unit(0.15, "cm")),
    linewidth = 0.4
  ) +
  scale_fill_gradient(low = "#fcbbbbad", high = "#ff3434d1") +
  labs(x = "", y = "", fill = "Wind Speed (m/s)") +
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )


# chlorophyll data
info(
  "noaacwNPPN20S3ASCIDINEOF2kmDaily",
  url = "https://coastwatch.noaa.gov/erddap/"
)

chlor_data <- griddap(
  "noaacwNPPN20S3ASCIDINEOF2kmDaily",
  time = c('2020-01-01', '2020-01-01'),
  latitude = c(-35, -29),
  longitude = c(16, 20),
  fields = "chlor_a",
  fmt = "csv",
  url = "https://coastwatch.noaa.gov/erddap/",
  store = memory()
) %>%
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
  dplyr::rename(
    t = time,
    lat = latitude,
    lon = longitude,
    chlo_a = chlor_a
  ) %>%
  na.omit()

p3 <- ggplot(chlor_data, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = chlo_a)) +
  annotation_borders(col = "black", fill = "cornsilk", linewidth = 0.6) +
  coord_equal(xlim = c(16, 20), ylim = c(-35, -29), expand = 0) +
  scale_fill_gradient(low = "#ffffffc1", high = "#0e7f08ff") +
  labs(
    x = "",
    y = "",
    fill = expression(paste(
      "Chl-a (mg/",
      m^{
        3
      },
      ")"
    ))
  ) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )


combined_plt <- ggarrange(p1, p2, p3, nrow = 1)

combined_plt_captioned <- annotate_figure(
  combined_plt,
  top = text_grob(
    "Temperature, Wind and Chlorophyll on the West Coast",
    face = "bold",
    size = 14
  ),
  bottom = text_grob(
    "All data presented from 01 January 2020",
    hjust = 0,
    x = 0.02,
    size = 10
  )
)

combined_plt_captioned

#ggsave(
#  "plots/west_coast_sst_wind_chla_map.png",
#  plot = combined_plt_captioned,
#  height = 6,
#  width = 9
#)
