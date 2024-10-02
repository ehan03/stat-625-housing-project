# Load libraries
library(sf)
library(dplyr)

# Load the property data with shapefile information
dg <- st_read("data/CT-parcel-data/4c5501b8-b68e-4888-bf6a-d92670d69c3b.gdb/")

# Create subset of single family homes, keep rest for matching purposes
h <- dg[which(dg$State_Use == "101"), ]

# Load SNAP info
s <- read.csv("data/CT_SNAP_Authorized_Retailers_20240920.csv")

# Convert to spatial
s_sf <- st_as_sf(s, coords = c("Longitude", "Latitude"), crs = 4326)

# Match pseudo-mercator of dg
s_sf <- st_transform(s_sf, st_crs(dg))