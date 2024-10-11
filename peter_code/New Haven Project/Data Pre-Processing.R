#' ---
#' title: "(1) Data Cleaning: Distance to Food and Housing Prices"
#' output:
#'   pdf_document:
#'     toc: true
#' ---
#' \newpage
#' 
#' # Description of This File
#' 
#' This file outlines the data cleaning process in five primary steps:
#' 
#' 1. **Sample Restriction**: We clean the data based on assumptions about the
#'    target population, requiring minimal assumptions about the data itself.
#' 2. **Data Processing**: We refine data by understanding the data-generating
#'    process for key variables and modifying variable types as needed.
#' 3. **Exploratory Data Analysis (EDA)**: We perform EDA to check that the
#'    data aligns with expectations, making adjustments where necessary.
#' 4. **Final Assessment**: We evaluate the dataset to quantify uncertainties
#'    and ensure data quality.
#' 5. **Data Saving**: We save the cleaned and processed dataset for analysis.
#' 
#' Below, we provide (1) a Project Description and (2) a Data Description to
#' guide our data cleaning steps clearly.
#' 
#' # Project Description
#' This project examines whether access to food, measured by proximity to 
#' grocery stores, impacts housing prices. We focus specifically on single 
#' family households in Connecticut to explore the relationship between 
#' housing prices and access to food resources.
#' 
#' # Data Description
#' 
#' This analysis uses two primary datasets:
#' 
#' 1. Housing data for Connecticut (in POLYGON format) and
#' 2. A directory of stores authorized to accept SNAP benefits, with 
#'    location coordinates (longitude and latitude).
#' 
#' The datasets are sourced from ATLAS. The housing data is spatially 
#' joined with store locations using the `st_nearest()` function, which 
#' calculates the nearest store to each housing location based on their 
#' geographic coordinates.
#' 
#' \newpage
#' # Data Pre-Processing
#' 
#' ## Import necessary Libraries and Data
library(sf)
#'
#' Load the dataset and drop unnecessary geometry for efficiency

# Load the Connecticut Property Shapefile
# d <- st_read("CT-parcel-data/4c5501b8-b68e-4888-bf6a-d92670d69c3b.gdb/")

# For Easy Data Handling
# d <- st_transform(d, crs = 4326)
# h <- st_drop_geometry(d)  # Drop geometry data since it's not needed
# rm(d) # remove for efficiency
# names(h) <- tolower(names(h))  # Convert column names to lowercase
# saveRDS("ct.rds")
d <- readRDS("ct.rds")

#'
#' ## Main Step (1) Sample Restricton
#' 
#' ### Data Filtering: Single-Family Homes in New Haven
#' 
#' We focus on single-family properties located in New Haven, CT. We refine 
#' the dataset by excluding:
#' 
#' 1. Extremely expensive properties (outliers that may skew analysis)
#' 2. Properties with a high number of rooms (likely multi-family/commercial)
#' 3. Properties without essential components like bedroom or bathrooms
#' 
#' These filters help reduce noise and exclude outliers. We avoid dropping 
#' NA values initially, as they may provide valuable information later.
#' 
#' 
#' 
#' ### Target Population: New Haven

# Filter for New Haven properties or missing town names (for inspection)
d <- d[d$town_name == "NEW HAVEN" | is.na(d$town_name), ]
nrow(d)  # Display the row count after filtering to verify reduction

#' 
#' 
#' ### Filtering by Number of Bedrooms and Bathrooms
#' 
#' We keep properties with fewer than 5 bedrooms. Properties with 0 bedrooms 
#' or bathrooms are inspected for data accuracy. Cross-checked entries show 
#' that some NA values were incorrectly converted to 0, so these entries 
#' are removed.
#' 
#' For more information, we have verified through Cross-checking with [Clear Vision](https://gis.vgsi.com/newhavenct/Parcel.aspx?pid=10772),
#' that many observations with 0 number of bedrooms and bathrooms have NA values. 
#' They have been converted to 0. 
#' 
#' Since (1) we want the number of bedrooms and bathrooms to be greater than 0
#' and (2) because the variables have non-trivial problems. We have removed them
#' from our data.

# Filter properties with fewer than 5 bedrooms
nrow(d)
sum(d$number_of_bedroom > 5, na.rm = TRUE)
d <- d[d$number_of_bedroom < 5 | is.na(d$number_of_bedroom), ]
head(d)  # Verify the dataset after filtering

#' Number of Bedrooms: While single family may include studios
#' we check whether the variable can be trusted. For instance, we are concerned
#' that NA value might have been translated to 0 values

# checked 4 WARWICK ST #12: it has 1 bath and 1 bed, data has problems
# checked 956 QUINNIPIAC AV: it is barren land
# checked 115 CRANSTON ST: it has 2 bed and 1 bath, data has problems
# checked 532 CHAPEL ST: it has 1 bed, 1 bath, 2 restrooms, data has problems
head(d[d$number_of_bedroom == 0 & !is.na(d$number_of_bedroom), ])

# Check properties with 0 bathrooms
# Checked 956 QUINNIPIAC AV: is not a house
# Checked 683 STATE ST #I: is a great unit, has 1 bath, data has problems
head(d[d$number_of_baths == 0 & !is.na(d$number_of_baths), ], 10)
# Checked 200 ORCHARD ST #408: is a great unit, has 1 bath, data has problems
# Checked 1395 CHAPEL ST: it's a dental building, it should have a bath!
tail(d[d$number_of_baths == 0 & !is.na(d$number_of_baths), ], 10)
sum(d$number_of_baths == 0, na.rm = T)

# Remove entries with 0 bedrooms and bathrooms as they don't meet criteria
sum(is.na(d$number_of_bedroom == 0))
d <- d[d$number_of_bedroom != 0 | is.na(d$number_of_bedroom), ]
sum(is.na(d$number_of_baths == 0))
d <- d[d$number_of_baths != 0 | is.na(d$number_of_baths), ]
nrow(d) # Check on the number of properties

#' ### Assessed Total: Price Restrictions
#' 
#' Set price limits for properties: below $50,000 likely indicate poor 
#' conditions, while those above $10 million may not reflect typical 
#' single-family homes. This step refines the dataset by removing extreme 
#' values.
#' 
#' **Check properties priced below $50,000**

# Check properties priced below $50,000
sum(d$assessed_total < 50000, na.rm = T)

# checked 1227 DEAN ST: looks like a nowhere
head(d[d$assessed_total < 50000 & !is.na(d$assessed_total), ])
# checked 747 WASHINGTON AV: it's a very old deli store
tail(d[d$assessed_total < 50000 & !is.na(d$assessed_total), ])

# Filter properties priced below $50,000
d <- d[d$assessed_total > 50000, ]
head(d)

#' **Check properties priced over $1M**

# Check properties priced over $1M
sum(d$assessed_total > 10000000, na.rm = T)
head(d[d$assessed_total > 10000000 & !is.na(d$assessed_total),])

# Filter properties priced over $1M
d <- d[d$assessed_total < 10000000, ]
head(d)

#' ### Living Area Restrictions
#' 
#' Set the living area to be above 0 and greater than 200 sq ft. This removes 
#' uninhabitable or unreasonably small properties that don't fit the criteria.

# Check properties with zero living area
# Checked 75 SOUTH END RD: it's a barren land
head(d[d$living_area == 0 & !is.na(d$living_area), ])
sum(d$living_area == 0, na.rm = T)
d <- d[d$living_area != 0 | is.na(d$living_area), ]
head(d)

# Check properties with less than 200 living area
sum(d$living_area < 200, na.rm = T)
# We do not want to live in areas that have less
d <- d[d$living_area > 200 | is.na(d$living_area), ]
head(d)

#' ### Filtering by Town Name
#' 
#' Properties with missing town names often have incomplete data. We verify 
#' and remove such entries after checking their attributes.

# Check properties with missing town names
nrow(d)
dnt <- d[is.na(d$town_name), ]
nrow(dnt)

# Inspect columns for properties with missing town names
# These properties are not real! They have no value
table(dnt$assessed_total, useNA = 'always')
table(dnt$number_of_bedroom, useNA = 'always')
table(dnt$number_of_baths, useNA = 'always')
table(dnt$zone, useNA = 'always')

# Remove entries with missing town names
d <- d[!is.na(d$town_name), ]

#' ## Main Step (2): Data Cleaning 
#'  
#' Note that definitions and categorization of single-family homes vary, 
#' so thorough examination is necessary to make reasonable classification 
#' decisions.

# `state_use_description` provides finer categorization than `state_use`
length(unique(d$state_use_description))
length(unique(d$state_use))

# List all state use descriptions for reference
sort(table(d$state_use_description, useNA = 'always'))

# Extract the single family homes using the regex exp
state_desc <- tolower(names(table(d$state_use_description, useNA = 'always')))
single_desc <- state_desc[grep("^(1 f|single|sf|one f|s f|sin|res single)", 
                               state_desc)]

# Filter single-family properties based on descriptions
c <- d[which(tolower(d$state_use_description) %in% single_desc), ]
table(c$state_use, useNA = 'always')
sort(table(c$state_use, useNA = 'always'))
sort(table(c$state_use_description, useNA = 'always'))

#' Investigate properties that are NOT classified as single-family. 
#' We need to evaluate whether properties like `Condominium` and `Apartment` 
#' can be reasonably included as single-family homes.

# Filter non-single-family properties for further inspection
ce <- d[!(tolower(d$state_use_description) %in% single_desc), ]
sort(table(ce$state_use))
sort(table(ce$state_use_description))


#' Inspect properties classified as `Condominium` or `Apartment`. 
#' These are attractive to single-family buyers, so they are manually checked.

# Check properties classified as apartments
ce_apt <- ce[ce$state_use_description %in% c("APT 4-Unit", 
                                             "APT5 - 12  MDL-94"), ]

# checked 490 WOODWARD AV: a great 2 bed - 1 bath apartment 
# checked 43 HARRINGTON AV: a great 2 bed - 1 bath apartment
# checked 165 MAIN ST ANNEX: a clean apartment! 
# checked  29 TERRACE ST: a great 2 bed - 1.5 bath apartments
# checked 70 HARRINGTON AV: a clean apartment!
head(ce_apt[!is.na(ce_apt$state_use_description), ])

# save the description labels for apartments
apt_desc <- state_desc[grep("^apt(?!t)", state_desc, perl = TRUE)]

# Confirm that apartments are legitimate properties
# checked 21 COLBY #CT: it's a great apartment!
# checked 145 COOPER PL: it's a great apartment
tail(d[tolower(d$state_use_description) %in% apt_desc, ])


# Check Condominium

# checked 675 TOWNSEND AV #116: a great 2 bed - 2 bath condominium
# checked 418 WOODWARD AV #26: a great 2 bed - 1 bath condominium
# checked 307 SAINT RONAN ST #A-7: a great 3 bed - 2 bath condominium
# (have walked across this property many times, many people at Yale
# would definitely consider living in this place)
ce_cond <- ce[ce$state_use_description %in% c("Condominium"), ]
head(ce_cond[!is.na(ce_cond$state_use_description), ])
tail(ce_cond[!is.na(ce_cond$state_use_description), ])
head(tail(ce_cond[!is.na(ce_cond$state_use_description), ], 1000))

# Save descriptions for condominiums
condo_desc <- state_desc[grep("^(condo)", state_desc, perl = TRUE)]

# Confirm that condominiums are legitimate propertiess
tail(d[tolower(d$state_use_description) %in% condo_desc, ])

# Create `sf` variable indicating single-family if description matches
d$sf <- ifelse(tolower(d$state_use_description) %in% 
                 c(single_desc, apt_desc, condo_desc), 1, 0)

# Verify if the classification works correctly
table(d$sf, d$town_name, useNA = 'always')

#' We conclude that `Residential` and `Condominiums` should be considered as 
#' single-family homes. This decision aligns with earlier restrictions 
#' applied to limit the number of rooms, bathrooms, and assessed total.
#' 
#' ## Step (3): Using EDA to further clean data
#' 
#' ### Assessed Total and Log-Transformation
#' Applying a log transformation on the `assessed_total` variable helps 
#' normalize the data distribution and enhances model performance.

# Save a new dataframe to isolate issues during the cleaning stage
dsf <- d

# Visualize the distribution of `assessed_total` before transformation
hist(dsf$assessed_total, main = "Histogram of Assessed Total")  
boxplot(dsf$assessed_total, main = "Boxplot of Assessed Total")

# Apply log transformation
dsf$at_log <- log(dsf$assessed_total)
hist(dsf$at_log, main = "Histogram of Logged Assessed Total")  

#' Observing the distribution, a long tail remains after transformation. 
#' We filter out properties priced over $2 million to remove extreme values,
#' which are not suitable for single family houses.

# Filter properties priced above $2 million
sum(dsf$assessed_total > 2000000)
dsf <- dsf[dsf$assessed_total < 2000000, ]

# Visualize the improved distribution after filtering
hist(dsf$at_log, main = "Histogram of Logged Assessed Total") 

#' ### Number of Rooms
#' 
#' Analyze room-related variables like bedrooms, bathrooms, and total rooms.

# Verify if any properties have 0 total rooms (should not be the case)
nrow(dsf)
sum(dsf$total_rooms == 0, na.rm = T)
range(dsf$total_rooms, na.rm = T)

# checked  480 WOODWARD AV : does not look like a home
# checked 83 EAST GRAND AV : it is a 4 bed - 2 bath home
# checked 70 FOXON ST: it is a container
# checked 41 TRUMBULL ST: it is a  2 bed - 1 bath apartment
# it might be possible that depending on the floor of this property,
# it may indicate basement and hence has no room 
# checked 842 HOWARD AV through vision solutions:
# we believe that it is using an algorithm to fill information
# we do not believe that this is accurate
head(dsf[dsf$total_rooms == 0 & !is.na(dsf$total_rooms), ])

# Remove entries with 0 total rooms due to data issues or non-residential use
dsf <- dsf[!(dsf$total_rooms == 0) | is.na(dsf$total_rooms), ]

# Remove properties with more than 10 total rooms as they may be outliers
sum(dsf$total_rooms > 10, na.rm = T)
dsf <- dsf[!(dsf$total_rooms > 10) | is.na(dsf$total_rooms), ]

# Verify the range after filtering
range(dsf$total_rooms, na.rm = T)

# Check for NA values in room-related variables
sum(is.na(dsf$total_rooms))
sum(is.na(dsf$number_of_bedroom))
sum(is.na(dsf$number_of_baths))
sum(is.na(dsf$number_of_half_baths))

# Check if NA values are in one or multiple columns for consistency
# That is, can we fix issues from other columns if we fix one?
# This may indicate that many of the missing value problmes may be an
# artifact of bad properties
temp <- dsf[!is.na(dsf$number_of_baths), ]
sum(is.na(temp$number_of_bedroom))
sum(is.na(temp$number_of_half_baths))

# Review entries with missing bedroom/bathroom values
head(dsf[is.na(dsf$number_of_bedroom), ])
head(dsf[is.na(dsf$number_of_baths), ])

#' Visualize room variable distributions

hist(dsf$total_rooms, main = "Histogram of Total Rooms")
hist(dsf$number_of_bedroom, main = "Histogram of Bedrooms")
hist(dsf$number_of_baths, main = "Histogram of Bathrooms")

#' Verify if `total_rooms` equals the sum of other room variables: it doesn't

head(dsf[, c('total_rooms', 'number_of_bedroom', 
             'number_of_baths', 'number_of_half_baths')], 5)

#' ### Property Size: Living Area and Effective Area

# Check for missing values in `living_area` and `effective_area`
nrow(dsf[is.na(dsf$living_area), ])  # Missing values in living_area
nrow(dsf[is.na(dsf$effective_area), ])  # Missing values in effective_area

# In some cases, living area matches effective area. To maintain consistency,
# focus on `living_area` and disregard `effective_area` to reduce NAs.
head(dsf[, c("living_area", "effective_area")])
tail(dsf[, c("living_area", "effective_area")])

# Living Area: Check the range and remove entries outside reasonable thresholds
range(dsf$living_area, na.rm = T)
sum(dsf$living_area < 200, na.rm = T)
sum(dsf$living_area > 10000, na.rm = T)

# Effective Area: Check the range and remove entries outside reasonable thresholds
range(dsf$effective_area, na.rm = T)
sum(dsf$effective_area < 200, na.rm = T)
sum(dsf$effective_area > 10000, na.rm = T)

# Checked 191 BURR S: a moderately big building and a huge lawn
# Checked 560 WOODWARD AV: a moderately big building and a huge lawn
# Checked 821 EAST SHORE PKWY: this is a huge barren land with buildings
head(dsf[dsf$living_area > 10000 & !is.na(dsf$living_area), ])
# Using effective area return similar properties 
head(dsf[dsf$effective_area > 10000 & !is.na(dsf$effective_area), ])

# Remove properties with living areas exceeding 10,000 sq ft
dsf <- dsf[dsf$living_area < 10000 | is.na(dsf$living_area), ]
nrow(dsf)

#' ### Effective Year Built (EYB)
#' EYB data shows inconsistencies; consider removing if it introduces bias.
table(dsf$ayb, useNA = 'always')
table(dsf$eyb, useNA = 'always')
hist(dsf$ayb, main = "Histogram of Actual Year Built (AYB)")
hist(dsf$eyb, main = "Histogram of Effective Year Built (EYB)")

# Check ranges and number of NA values
range(dsf$eyb, na.rm = T)  # eyb is not consistent at all
range(dsf$ayb, na.rm = T)  # ayb has fewer data problems
sum(is.na(dsf$eyb))
sum(is.na(dsf$ayb))

#' We decide to use `AYB` since it is more reliable. 
#' 
#' ### Zone Descriptions
#' 
#' We categorize zones to understand their effects on housing prices. Zone 
#' descriptions and codes are grouped based on zoning regulations using 
#' resources like (1) Municode Library and (2) New Haven Zoning and 
#' Regulations. In some cases, we also used AI and online searches to 
#' categorize zones when direct matches were not available. While this 
#' grouping may not be entirely accurate, zoning likely affects housing 
#' prices, so it is included in the analysis

# Tabulate zone types and descriptions
sort(table(dsf$zone_description, useNA = 'always'))
sort(table(dsf$zone, useNA = 'always'))
sum(is.na(dsf$zone_description))  
sum(is.na(dsf$zone))  
zones <- names(table(dsf$zone))

# 1. Residential Zones
residential_zones <- zones[grep("^RS|RM", zones)]

# 2. Commercial Zones
commercial_zones <- zones[grep("^BA|^BB|^BD", zones)]

# 3. Mixed-Use Zones
mixed_use_zones <- zones[grep("/", zones)]

# 4. Industrial Zones
industrial_zones <- zones[grep("^IL|^IH", zones)]

# 5. Planned Development Districts (PDD)
pdd_zones <- zones[grep("^PDD", zones)]

# 6. Public Use Zones (PARK, CEM)
public_use_zones <- zones[grep("PARK|CEM", zones)]

# 7. Specialized Residential/Office Zones
special_res_office_zones <- zones[grep("RO", zones)]

# 8. Parking and Utility Zones (PDU)
parking_utility_zones <- zones[grep("^PDU", zones)]

# 9. Historic and Overlay Zones
historic_overlay_zones <- zones[grep("RH", zones)]

# 10. Undefined/Other Zones
undefined_other_zones <- zones[!zones %in% c(residential_zones, commercial_zones, 
                                             mixed_use_zones, industrial_zones, 
                                             pdd_zones, public_use_zones, 
                                             special_res_office_zones, 
                                             parking_utility_zones, 
                                             historic_overlay_zones)]

# Reclassify zone variable based on categories
dsf$zone <- ifelse(dsf$zone %in% residential_zones, "Residential", dsf$zone)
dsf$zone <- ifelse(dsf$zone %in% commercial_zones, "Commercial", dsf$zone)
dsf$zone <- ifelse(dsf$zone %in% mixed_use_zones, "Mixed", dsf$zone)
dsf$zone <- ifelse(dsf$zone %in% industrial_zones, "Industrial", dsf$zone)
dsf$zone <- ifelse(dsf$zone %in% pdd_zones, "Planned Development", dsf$zone)
dsf$zone <- ifelse(dsf$zone %in% public_use_zones, "Public Use", dsf$zone)
dsf$zone <- ifelse(dsf$zone %in% special_res_office_zones, "Speical Res", dsf$zone)
dsf$zone <- ifelse(dsf$zone %in% parking_utility_zones, "Parking", dsf$zone)
dsf$zone <- ifelse(dsf$zone %in% historic_overlay_zones, "Historic", dsf$zone)
dsf$zone <- ifelse(dsf$zone %in% undefined_other_zones, "Others", dsf$zone)

# Tabulate the reclassified zones
table(dsf$zone)

#' ### Condition Description
#' 
#' We inspect the condition descriptions

# Inspect condition descriptions
table(dsf$condition_description, useNA = 'always')
length(table(dsf$condition_description))

# Reclassify conditions where necessary
dsf$condition_description <- ifelse(dsf$condition_description == "F", "Fair", 
                                    dsf$condition_description)


# Does "BA" mean bad? NO
# Checked properties to confirm
# Checked 46 CLINTON AV: warehouse
# Checked 102 FOOD TERMINAL PLZ: warehouse
head(dsf[dsf$condition_description == "BA" & 
           !is.na(dsf$condition_description), ])

# Does F mean Fair? YES
# Does "F" mean Fair? Checked properties to confirm
# Checked 150 HUNTINGTON RD: fair condition (our judgement)
# Checked 204 BURR ST: fair condition (our judgement)
# Checked 351 CONCORD ST: fair condition (our judgement)
head(dsf[dsf$condition_description == "Fair" 
         & !is.na(dsf$condition_description), ])

# Remove entries with "BA" as it doesn't match our criteria
dsf <- dsf[dsf$condition_description != "BA" | 
             is.na(dsf$condition_description), ]

# Convert condition_description to a factor variable
dsf$condition_description <- factor(dsf$condition_description, 
                                    levels = c("Very Poor", "Poor", "Fair",
                                               "Average", "Good", "Very Good",
                                               "Excellent"))

# Verify that the factor levels are set correctly
levels(dsf$condition_description)

# Assess missing values in condition_description
nrow(dsf[is.na(dsf$condition_description), ])
head(dsf[is.na(dsf$condition_description), ])

#' ## STEP (4) Final Check: 
#' Assessing and Understanding Data Quality 

# Condition Description
unique(dsf$condition_description)
sum(is.na(dsf$condition_description))
table(dsf$condition_description, useNA = "always")
head(dsf[is.na(dsf$condition_description), ])

# Number of Rooms: Bedrooms, Bathrooms, Half Baths
sum(is.na(dsf$total_rooms))
sum(is.na(dsf$number_of_bedroom))
sum(is.na(dsf$number_of_baths))
sum(is.na(dsf$number_of_halfbaths))

# Zone
sum(is.na(dsf$zone))

# Living Area 
sum(is.na(dsf$living_area))

# Assessed Total
sum(is.na(dsf$assessed_total))


#' ## STEP(5): Select Variables for Analysis and Save the data
#' 
#' Select key variables for analysis to simplify the data processing

# Key Variables of Interests
vars_interests <- c(
  "at_log", "assessed_total", "zone",
  "condition_description", "ayb", "total_rooms", 
  "number_of_bedroom", "number_of_baths", 
  "living_area"
)


# Key Variables of Distance 
dist_vars <- c(
  "dist_convenience_store", "dist_farmers_and_markets", 
  "dist_grocery_store", "dist_super_store", 
  "dist_supermarket", "area_super_store", 
  "area_supermarket", "area_grocery_store"
)

# Select relevant columns for analysis
dr <- dsf[, vars_interests]  
saveRDS(dr, "cleaned.rds")

#' \newpage
#' # Conclusion
#' 
#' In this file, we have cleaned the dataframe
#' 
#' # Sessioninfo
#' 
sessionInfo()
#' 
#' \newpage
#' # Appendix (Other information)
#' 
#' 
#' ### Description of Zones:
#' 
#' I found the definitions and zoning regulations for New Haven, CT, 
#' which will help categorize the zones into about 10 categories. 
#' Here's an overview:
#' 
#' 1. **Residential Zones (RS, RM)**: These zones include various types of 
#' residential areas like single-family (RS1, RS2) and multi-family (RM1, RM2). 
#' They typically differ based on density and the type of dwellings allowed.
#' 2. **Commercial Zones (BA, BB, BD)**: These include zones designated for 
#' businesses and commercial activities (e.g., BA, BD, BB). The differences 
#' often relate to the scale and type of commercial development permitted.
#' 3. **Mixed-Use Zones (e.g., BA/RM1, RM2/RO)**: These zones allow a 
#' combination of residential and commercial or other uses, promoting 
#' development that integrates living, working, and recreational spaces.
#' 4. **Industrial Zones (IL, IH)**: These zones are for light (IL) and 
#' heavy (IH) industrial activities. They regulate the types of manufacturing 
#' and industrial processes allowed.
#' 5. **Planned Development Districts (PDD)**: These are special districts 
#' (e.g., PDD 45, PDD 53) created for specific development projects or areas 
#' that require unique regulations not covered by standard zoning categories.
#' 6. **Public Use Zones (PARK, CEM)**: These zones are designated for public 
#' amenities like parks and cemeteries, preserving open spaces for recreational 
#' and community purposes.
#' 7. **Historic and Overlay Zones**: Some areas may have historic designations 
#' or specific overlays that impose additional regulations to maintain certain 
#' aesthetic or architectural standards.
#' 8. **Specialized Residential/Office Zones (RO)**: These zones accommodate 
#' both residential and office spaces, providing flexibility in usage depending 
#' on the location and needs of the area.
#' 9. **Parking and Utility Zones (PDU)**: Zones such as PDU are designated for 
#' parking or specific utility-related uses, ensuring infrastructure support for 
#' residential and commercial areas.
#' 10. **Unique or Undefined Categories**: Some combinations like BA/RO or 
#' RM1/RS2 may combine characteristics of multiple zones. They are typically 
#' used to allow flexibility in transitioning areas.
#' 
#' Reference Resources
#' - [Municode](tps://library.municode.com/ct/new_haven/codes/zoning).
#' - [New Haven Zoning and Regulations](https://www.newhavenct.gov/government/departments-divisions/city-plan/zoning-and-regulations)

