library(rerddap)
library(dplyr)
library(ggplot2)


# search for data sets
data <- rerddap::ed_search("MUR", url = "https://coastwatch.noaa.gov/erddap/")

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
  latitude = c(-40, -30),
  longitude = c(15, 30),
  fields = "analysed_sst",
  fmt = "csv",
  url = "https://coastwatch.noaa.gov/erddap/",
  store = memory()
)

# well...that simply worked.

# Neaten the data a bit
sst_data <- sst_data %>%
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
  dplyr::rename(
    t = time,
    lat = latitude,
    lon = longitude,
    sst = analysed_sst
  ) %>%
  na.omit()

# PLot
ggplot(sst_data, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = sst))
