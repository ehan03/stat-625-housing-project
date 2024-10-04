#' ---
#' title: "Distance to Food and Housing Prices"
#' output:
#'   pdf_document:
#'     toc: true
#' ---
#' \newpage
#' 
#' # Project: Distance to Food and Housing Prices
#' 
#' # Project Background
#' 
#' ## Project Description
#' This project investigates whether access to food, measured by distance 
#' to nearby grocery stores, affects housing prices. We focus on single 
#' households in Connecticut to explore the relationship between housing 
#' prices and access to food. 
#' 
#' ## Data Description
#' 
#' This project utilizes two main datasets: 
#' 
#' * (1) housing data for the state of Connecticut and 
#' * (2) a directory of stores authorized to accept SNAP benefits 
#' 
#' obtained from ATLAS. The housing data is in POLYGON format, while the 
#' store data includes longitude and latitude coordinates. By leveraging the 
#' spatial geometry of both datasets, we merged them using the `st_nearest()` 
#' function, which calculates the nearest points between the housing 
#' locations and stores based on their geographic coordinates.
#' 
#' ## Model Description
#' We will use a linear model to explore the relationship between housing 
#' prices and the following variables:
#' 
#' 1. **Rooms**: Bedrooms, bathrooms, half-baths, total rooms. We expect a 
#'    positive relationship between housing prices and the number of rooms.
#' 
#' 2. **Size**: Property size, where larger properties are expected to have 
#'    higher prices. However, we avoid farm properties by focusing on single 
#'    households to mitigate concerns about misclassification.
#' 
#' 3. **Zone Types**: Different zoning districts that regulate property use, 
#'    including whether commercial uses like restaurants are allowed. This 
#'    factor can influence housing prices.
#' 
#' 4. **Actual Year Built (AYB)**: Property age based on its actual year 
#'    built. We expect older properties may be less expensive, depending on 
#'    their renovations and current condition.
#' 
#' 5. **Condition**: Properties classified by condition (e.g., Excellent, 
#'    Poor) may show substantial variation in prices.
#' 
#' \newpage
#' # Data Pre-Processing
#' 
#' ## Import Libraries and Data
library(sf)

#' Load the dataset and drop unnecessary geometry for efficiency

# Load the Connecticut Property Shapefile
# d <- st_read("CT-parcel-data/4c5501b8-b68e-4888-bf6a-d92670d69c3b.gdb/")

# For Easy Data Handling
# d <- st_transform(d, crs = 4326)
# dng <- st_drop_geometry(d)  # Drop geometry data since it's not needed
# rm(d) # remove for efficiency
# names(dng) <- tolower(names(dng))  # Convert column names to lowercase

dng <- readRDS("ct.rds")

#' ## Exploratory Data Analysis
#' 
#' ### Assessed Total and Log-Transformation
#' Log-transforming the `assessed_total` variable helps achieve normality 
#' and produces better model results.
#' 

# filter out observations with less than 0 total values
head(dng[dng$assessed_total <= 0 | is.na(dng$assessed_total), ])  # off market? 
dng <- dng[!dng$assessed_total <= 0 | is.na(dng$assessed_total), ]

# There are many missing values.
# While I would hesitate to drop the missing values,
# this variable is important!
# in retrospect, this solves many other NA issues in other
# variables. 
sum(is.na(dng$assessed_total))
# Verify the following decision by Prof. Jay
dng <- dng[!is.na(dng$assessed_total), ]

# There are bad properties
range(dng$assessed_total, na.rm = TRUE)
nrow(dng[dng$assessed_total < 1000 & !is.na(dng$assessed_total), ])
nrow(dng[dng$assessed_total > 300000000 & !is.na(dng$assessed_total), ])

# Reasonable filtering
dng <- dng[dng$assessed_total > 1000 | is.na(dng$assessed_total), ] 
dng <- dng[dng$assessed_total < 300000000 | is.na(dng$assessed_total), ]

# see the distribution
hist(dng$assessed_total, main = "Histogram of Assessed Total")  # Original

# There are some bad data
# we are going to revisit this
nrow(dng[dng$assessed_total < 10000 & !is.na(dng$assessed_total), ])
head(dng[dng$assessed_total < 10000 & !is.na(dng$assessed_total), ])
tail(dng[dng$assessed_total < 10000 & !is.na(dng$assessed_total), ])

# Take the log-transform
dng$at_log <- log(dng$assessed_total)

# current look
hist(dng$at_log, main = "Histogram of Log-transformed Assessed Total")


#' ### Number of Rooms
#' Explore the distribution of room-related variables (bedrooms, bathrooms, 
#' half-baths, total rooms).

#' Check for NA values
#' While NA values make sense,
#' 0 total number of rooms do not make sense
nrow(dng[is.na(dng$total_rooms), ])  

# observer further
head(dng[dng$total_rooms == 0, ])
tail(dng[dng$total_rooms == 0, ])
range(dng$total_rooms, na.rm = T)

# We are confident about these choices
dng <- dng[!(dng$total_rooms == 0) | is.na(dng$total_rooms), ]
dng <- dng[!(dng$total_rooms > 400) | is.na(dng$total_rooms), ]

# Ask Prof. Jay for the following decision
# dng <- dng[!(is.na(dng$total_rooms)), ]

# The data looks much better
range(dng$total_rooms, na.rm = T)

# Now let's inspect others
sum(is.na(dng$total_rooms))
sum(is.na(dng$number_of_bedroom))
sum(is.na(dng$number_of_baths))
sum(is.na(dng$number_of_half_baths))

# do we observe NA values in both columns?
# or just a single column? 
temp <- dng[!is.na(dng$number_of_baths), ]
sum(is.na(temp$number_of_bedroom))

# Let's inspect
# Let's delay our decisions to drop the data yet
head(dng[is.na(dng$number_of_bedroom), ])
head(dng[is.na(dng$number_of_baths), ])

#' Let's see histograms
hist(dng$total_rooms, main = "Histogram of Total Rooms")
hist(dng$number_of_bedroom, main = "Histogram of Bedrooms")
hist(dng$number_of_baths, main = "Histogram of Bathrooms")

#' Check if `total_rooms` equals the sum of other room variables (it doesn’t)
head(dng[, c('total_rooms', 'number_of_bedroom', 
             'number_of_baths', 'number_of_half_baths')], 5)

#' ### Property Size
#' Check for missing values in `living_area` and `effective_area`
nrow(dng[is.na(dng$living_area), ])  # Missing values in living_area
nrow(dng[is.na(dng$effective_area), ])  # Missing values in effective_area

# Inspect
head(dng[is.na(dng$effective_area), ])

# For some properties, living area is the same as
# effective area. For consistency, we can consider
# living area and disregard effective area and 
# partially obviate its NA problem
head(dng[, c("living_area", "effective_area")])
tail(dng[, c("living_area", "effective_area")])

#' ### Living Area
range(dng$living_area, na.rm = T)
dng <- dng[dng$living_area > 10 | is.na(dng$living_area), ]
dng <- dng[dng$living_area < 1000000 | is.na(dng$living_area), ]

#' ### Effective Year Built (EYB)
#' Explore the distribution of property age based on renovations and updates
hist(dng$eyb, main = "Histogram of Effective Year Built (EYB)")
range(dng$eyb, na.rm = T)  # eyb is not consistent at all
range(dng$ayb, na.rm = T)  # ayb has fewer data problems

# Let's check the number of NA values in each var
sum(is.na(dng$eyb))
sum(is.na(dng$ayb))

# While it is true that `eyb` provides other information than `ayb`. 
# The lack of consistency across the dataset may introduce other biases
# that we may not want in our data. Therefore, we do not consider `eyb` 
# in our dataset.

#' Is it going to be worthwhile to drop `ayb`? It seems to tell us
#' important information! We need to think more
 
#' ### Zone Descriptions
#' Tabulate zone types and their descriptions to understand the zoning 
#' districts' effects on housing prices.
table(dng$zone_description) 
table(dng$zone)
sum(is.na(dng$zone_description))  
sum(is.na(dng$zone))  

# clean up the zone
dng$zone <- gsub("\\.0$", "", dng$zone)
table(dng$zone)
length(table(dng$zone))

# inspect more
table(dng$condition_description)
length(table(dng$condition_description))

# Convert condition descriptions to lowercase
dng$condition_description <- tolower(dng$condition_description)

# Clean up variations of 'average', 'good', 'very good', 'fair', 'poor', and other categories
dng$condition_description <- gsub(
  "avarage|average-|average\\+|avg-good|av|averageerage|average / fair|average/fair|average-good", 
  "average", 
  dng$condition_description
)

# Clean up variations of 'good'
dng$condition_description <- gsub("good|g\\+|gd|good/very good", "good", 
                                  dng$condition_description)

# Clean up variations of 'very good'
dng$condition_description <- gsub("vgood|good-vg|vg\\+|very good|vg|very good/excellent|very good/good", 
                                  "very good",  
                                  dng$condition_description)

# Clean up variations of 'fair'
dng$condition_description <- gsub("^f$|fair|fair-avg|fair-average|fr|fair-averageg|fair/poor", 
                                  "fair", 
                                  dng$condition_description)

# Clean up variations of 'poor'
dng$condition_description <- gsub("poor|pr|poor / fair", "poor", 
                                  dng$condition_description)

# Clean up 'unsound' and 'delapitated'
dng$condition_description <- gsub("delapitated|unsound", "unsound", 
                                  dng$condition_description)

# Clean up 'excellent'
dng$condition_description <- gsub("excellent|ex|excellent/very good", "excellent", 
                                  dng$condition_description)

# Clean up 'remodeled' and 'renovated'
dng$condition_description <- gsub("remodeled|renovated", "renovated", 
                                  dng$condition_description)


# check
table(dng$condition_description)


# Additional Manipulations: Poor
dng$condition_description <- ifelse(
  dng$condition_description %in% c("ba"), 
  "poor", 
  dng$condition_description
)


# Additional Manipulations: poor
dng$condition_description <- ifelse(
  dng$condition_description %in% c("fair / poor"), 
  "poor", 
  dng$condition_description
)


# Additional Manipulations: fair
dng$condition_description <- ifelse(
  dng$condition_description %in% c("averageerage/ fair", "f+",
                                   "fair / averageerage"), 
  "fair", 
  dng$condition_description
)

# Additional Manipulations: average
dng$condition_description <- ifelse(
  dng$condition_description %in% c("averageerage", "averageerage / good",
                                   "averageerage/good", "normal"), 
  "average", 
  dng$condition_description
)

# Additional Manipulations: good
dng$condition_description <- ifelse(
  dng$condition_description %in% c("good / very good", "good+", "g-",
                                   "very good / good", "very good/good",
                                   "aa"), 
  "good", 
  dng$condition_description
)

# Additional Manipulations: very good 
dng$condition_description <- ifelse(
  dng$condition_description %in% c("very good / excellent", "very good/excellent",
                                   "very good/excellentcel", " very good"), 
  "very good", 
  dng$condition_description
)


# Very bad behaviors
head(dng[dng$condition_description == "r", ])
head(dng[dng$condition_description == "rb", ])
tail(dng[dng$condition_description == "rb", ])
head(dng[dng$condition_description == "re", ])
head(dng[dng$condition_description == "u", ])
head(dng[dng$condition_description == "uc", ])
head(dng[dng$condition_description == "unsound", ])
head(dng[dng$condition_description == "renovated", ])
head(dng[dng$condition_description == "dilapidated", ])


# Remove any problematic values from analysis
dng <- dng[!(dng$condition_description %in% c("r", "rb", "re", "u", "unsound",
                                              "uc", "dilapidated", "renovated")), ]

# check
table(dng$condition_description)
head(dng[dng$condition_description == "0" & !is.na(dng$condition_description), ], 5)
head(dng[dng$condition_description == "1" & !is.na(dng$condition_description), ], 3)



# Making some arbitrary decisions
# was not able to find these data on the internet, unfortunately
dng$condition_description[dng$condition_description == "0"] <- "very poor"
dng$condition_description[dng$condition_description == "1"] <- "poor"
dng$condition_description[dng$condition_description == "2"] <- "fair"
dng$condition_description[dng$condition_description == "3"] <- "average"
dng$condition_description[dng$condition_description == "4"] <- "good"
dng$condition_description[dng$condition_description %in% c("5", "6")] <- "very good"
dng$condition_description[dng$condition_description %in% c("7", "8")] <- "excellent"

# Check the results
table(dng$condition_description)

# Convert back to sentence case
dng$condition_description <- tools::toTitleCase(dng$condition_description)


# Convert condition_description to a factor variable with ordered levels
dng$condition_description <- factor(dng$condition_description, 
                                    levels = c("Very Poor", "Poor", "Fair", 
                                               "Average", "Good", 
                                               "Very Good", "Excellent"))

#' ## Final Check: Assessing and Understanding Data Quality

# Condition Description
sum(is.na(dng$condition_description))
table(dng$condition_description, useNA = "always")
head(dng[is.na(dng$condition_description), ])

# Number of Rooms
sum(is.na(dng$total_rooms))
sum(is.na(dng$number_of_bedroom))
sum(is.na(dng$number_of_baths))
sum(is.na(dng$number_of_halfbaths))

# Zone
sum(is.na(dng$zone))

# Living Area Size
sum(is.na(dng$living_area))

# Assessed Total
sum(is.na(dng$assessed_total))


#' ## Data Selection for Analysis
#' Select key variables for analysis to simplify the data processing
vars_interests <- c(
  "at_log", "assessed_total", "zone",
  "condition_description", "ayb", "total_rooms", 
  "number_of_bedroom", "number_of_baths", 
  "living_area"
)

dist_vars <- c(
  "dist_convenience_store", "dist_farmers_and_markets", 
  "dist_grocery_store", "dist_super_store", 
  "dist_supermarket", "area_super_store", 
  "area_supermarket", "area_grocery_store"
)


dr <- dng[, vars_interests]  # Select relevant columns


#' ## Exploratory Bivariate Plots
#' Create scatter plots and boxplots to explore relationships between 
#' key variables and housing prices (log-transformed).
plot(dr$total_rooms, dr$at_log, main = "Total Rooms vs Log-Assessed Total")  
plot(dr$number_of_bedroom, dr$at_log, main = "Bedrooms vs Log-Assessed Total")
plot(dr$number_of_baths, dr$at_log, main = "Baths vs Log-Assessed Total")
plot(dr$ayb, dr$at_log, main = "AYB vs Log-Assessed Total")  
boxplot(dr$at_log ~ dr$condition_description, 
        main = "Boxplot of Log-Assessed Total by Condition")



#' \newpage
#' # Modeling
#' 
#' ## Base Model decisions
#' 
#' ### Linear models to explore relationships
#' Base model: rooms, condition, and zone on logged assessed total
mod_b <- lm(at_log ~ total_rooms + number_of_bedroom + number_of_baths + 
              + condition_description + zone + living_area, data = dr)

# save the summary
mod_b_summary <- summary(mod_b)

# Exclude zone coefficients from the summary
zone_coefs <- grep("^zone", rownames(mod_b_summary$coefficients))
mod_b_summary$coefficients <- mod_b_summary$coefficients[-zone_coefs, ]

# Display the modified summary
mod_b_summary


#' ### Mixed Effects model
library(lme4)

#' I think we should treat zone as a random effect because it allows us to 
#' model the variability across many zones without estimating a separate 
#' coefficient for each, ensuring the model remains generalizable and avoids 
#' overfitting. Using zone as a fixed effect would lead to estimating over 
#' 100 coefficients, making the model unnecessarily complex and less 
#' applicable to zones outside the dataset.

# Use zone as a random effect
mod_b_mixed <- lmer(at_log ~ total_rooms + number_of_bedroom + 
                      number_of_baths + condition_description + 
                      (1 | zone) + living_area, data = dr)
summary(mod_b_mixed)


#' \newpage
#' # Conclusion
#' The distance to supermarkets and other food sources shows a potential 
#' non-linear effect on housing prices, with squared distance terms providing 
#' additional insights into the relationship.
#' 
#' # Sessioninfo
sessionInfo()
#' 
#' \newpage
#' # Appendix (Other information)
#' 
#' 
#' ### Description of Zones:
#' 
#' **CGB (Central Greenwich Business)**: Commercial zone for businesses located 
#' in the central part of Greenwich.
#' 
#' **GB (General Business)**: General business district, allowing a variety of 
#' commercial uses.
#' 
#' **GBO (General Business Office)**: A zone designated for office buildings 
#' within a general business area.
#' 
#' **LB (Local Business)**: Zones that permit small-scale, neighborhood-serving 
#' businesses.
#' 
#' **LBR-1 (Local Business Retail 1)**: Retail-focused business zone, typically 
#' for smaller shops or services.
#' 
#' **LBR-2 (Local Business Retail 2)**: Another retail business zone, with 
#' slightly different regulations or area requirements than LBR-1.
#' 
#' **R-12 (Single Family 12,000 sq. ft.)**: Residential zone for single-family 
#' homes on lots with a minimum of 12,000 square feet.
#' 
#' **R-20 (Single Family 20,000 sq. ft.)**: Residential zone for single-family 
#' homes on larger lots with a minimum of 20,000 square feet.
#' 
#' **R-6 (Multi-Family 7,500 sq. ft.)**: Residential zone for multi-family 
#' housing on smaller lots of at least 7,500 square feet.
#' 
#' **R-7 (Single Family 7,500 sq. ft.)**: Residential zone for single-family 
#' homes on smaller lots of at least 7,500 square feet.
#' 
#' **R-C12 (Residential Conservation)**: Conservation-focused residential zone 
#' with larger lot sizes (likely 12,000 # sq. ft. or more).
#' 
#' **R-C20 (Residential Conservation)**: Conservation-focused residential zone 
#' with larger lot sizes (likely 20,000 # sq. ft. or more).
#' 
#' **R-C7 (Residential Conservation)**: Another conservation-focused residential 
#' zone, likely around 7,500 sq. ft.
#' 
#' **R-CC4 (Residential Conservation Cluster)**: Cluster development that 
#' conserves open space while permitting residential  homes, typically with 
#' reduced lot sizes.
#' 
#' **R-MF (Single & Multi-Family)**: A residential zone allowing both 
#' single-family and multi-family dwellings.
#' 
#' **R-PHD-E (Elderly Planned Housing)**: Housing planned specifically for 
#' elderly residents.
#' 
#' **R-PR (Planned Residential)**: A planned residential development, likely 
#' involving some sort of mixed-use or non-traditional layout.
#' 
#' **RA-1 (Single Family 1 acre)**: Residential zone for single-family homes on 
#' lots of at least 1 acre.
#' 
#' **RA-2 (Single Family 2 acre)**: Residential zone for single-family homes on 
#' lots of at least 2 acres.
#' 
#' **RA-4 (Single Family 4 acre)**: Residential zone for single-family homes on 
#' lots of at least 4 acres.
#' 
#' **RA-C1/RA-C2/RA-C4**: Conservation-focused residential zones, with varying 
#' minimum lot sizes for conservation purposes.
#' 
#' **WB (Waterfront Business)**: Business zone designed for properties located 
#' on the waterfront, typically  permitting commercial uses related to water 
#' activities.

          