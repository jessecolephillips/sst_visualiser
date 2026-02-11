library(rerddap)


rerddap::ed_datasets(url = "https://coastwatch.noaa.gov/erddap/")


data <- ed_search("MUR", url = "https://coastwatch.noaa.gov/erddap/")

# SO, rerrdap DOES work, but you MUST use the correct URLs, or things crash.
