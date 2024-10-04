# Load libraries
library(sf)

# Load the property data with shapefile information
dg <- st_read("data/CT-parcel-data/4c5501b8-b68e-4888-bf6a-d92670d69c3b.gdb/")
dg <- st_transform(dg, crs = 4326)

# Create subset of single family homes
# State use of 101 corresponds to single family homes -- state use description
# is unreliable (many different variations that have functionally same meaning)
h <- dg[which(dg$State_Use == "101" & !is.na(dg$Assessed_Total) & 
              dg$Assessed_Total > 0), ]

# Very long tails on both sides, lower tail particularly concerning
hist(log(h$Assessed_Total))

# Check out some of the extrema
# Look at properties with low assessed values
h[which(rank(h$Assessed_Total) < 10), ]

# We notice a common pattern, many of these have assessed building value of 0
# 9 WATER ST for example is a parking lot
# Intuitively, assessed building value of 0 should mean there isn't actually a
# residential building
h <- h[which(h$Assessed_Building > 0), ]

# Look at properties with low assessed values again
# 13 NEW HAVEN AVE for example
# Assessed total = $7,210
# Legit according to Zillow
# https://www.zillow.com/homedetails/13-New-Haven-Ave-Plainville-CT-06062/61802820_zpid/
h[which(rank(h$Assessed_Total) < 10), ]

# Largest assessed value = $29,718,710
# INDIAN POINT LANE 0034
# https://www.zillow.com/homedetails/34-Indian-Point-Ln-Riverside-CT-06878/57312297_zpid/
h[which.max(h$Assessed_Total), c("Town_Name", "Location", "Assessed_Total")]

# Revisit histogram post-filtering
# Makes some more sense now!
hist(log(h$Assessed_Total))


# Consider following columns for now and relationship with target:
# - Town_Name
# - Zone
# - Condition
# - Living_Area
# - Effective_Area
# - Total_Rooms
# - Number_of_Bedroom
# - Number_of_Baths
# - Number_of_Half_Baths

# Living_Area
# Why are there residences with a living area of 0??
# 194 CRESTWOOD TERR for example
# Nonzero living area
# https://gis.vgsi.com/orangect/Parcel.aspx?pid=4107
h[which(h$Living_Area == 0), ]

# Set living area to NA for these
h[which(h$Living_Area == 0), "Living_Area"] <- NA

# Investigate lower tail
hist(log(h$Living_Area))
h[which(rank(h$Living_Area) < 10), ]


plot(log(h$Living_Area), log(h$Assessed_Total))


# Condition
# Can we standardize the values using condition description + best judgement?
unique(h$Condition)
unique(h$Condition_Description)




# Load SNAP info
s <- read.csv("data/CT_SNAP_Authorized_Retailers_20240920.csv")

# Convert to spatial and match pseudo-mercator of dg
s_sf <- st_as_sf(s, coords = c("Longitude", "Latitude"), crs = 4326)
s_sf <- st_transform(s_sf, st_crs(dg))

# Create features from SNAP info and explore
# Some ideas:
# - Distances to nearest store of each type
# - Some heuristic for 'density' of food within a radius/multiple radii
# - Prevalence of certain store types within a radius/multiple radii