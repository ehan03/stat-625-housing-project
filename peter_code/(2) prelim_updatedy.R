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
# h <- st_drop_geometry(d)  # Drop geometry data since it's not needed
# rm(d) # remove for efficiency
# names(h) <- tolower(names(h))  # Convert column names to lowercase
d <- readRDS("ct.rds")


#' ## Resonable Sample Restriction
#' 
#' 
#' Recall that we are interested in home properties for single families. 
#' As we have previously discussed, it is non-sensical to expect that 
#' houses that are (1) substantially expensive, (2) has an absurd number of
#' rooms, (3) has no bathroom, and (4) has no living room does not make sense.
#' 
#' Since these are our `Target Population`, we first clean the dataset to
#' fit our needs. This section, we need relatively little data check since
#' we are making sample selection based on what we believe are reasonable
#' conditions for single family households. 
#' 
#' This step is taken first to reduce additional complexities we face that
#' might be driven by properties that have a substantial number of rooms,
#' expensive price tag, among others.
#' 
#' 
#' 
#' ### Number of Bedrooms and Number of Bathrooms
nrow(d)
sum(d$number_of_bedroom > 5, na.rm = T)
d <- d[d$number_of_bedroom < 5 | is.na(d$number_of_bedroom), ]
head(d)



# Checked 59 BECKWITH RD: does not look like a house
# Checked 98 NORTHROP RD: is not a house
# Checked 730 AMITY RD: this is a hoop house
head(d[d$number_of_baths == 0 | is.na(d$number_of_baths), ], 10)
sum(d$number_of_baths == 0, na.rm = T)
d <- d[d$number_of_baths > 0, ]
head(d)


#' ### Assessed Total

# We want reasonably priced houses
nrow(d)
sum(d$assessed_total < 50000, na.rm = T)
d <- d[d$assessed_total > 50000, ]
head(d)

# We do not want too expensive houses 
nrow(d)
sum(d$assessed_total > 10000000, na.rm = T)
d <- d[d$assessed_total < 10000000, ]
head(d)

#' ### Living Area
#' 

# Checked 43 EAST SHORE DR: This looks like a garage
# Checked 196 HIDDEN LAKE RD: This definitely is not a house 
head(d[d$living_area == 0 & !is.na(d$living_area), ])
sum(d$living_area == 0, na.rm = T)
d <- d[d$living_area != 0, ]
head(d)


# We do not want to live in areas that have less
# than 200 square feet
sum(d$living_area < 200, na.rm = T)
d <- d[d$living_area > 200 | is.na(d$living_area), ]
head(d)


#' ### We need to inspect the town name
#' We can be confident about dropping thsese observations
nrow(d)
dnt <- d[is.na(d$town_name), ]
nrow(dnt)
table(dnt$assessed_total, useNA = 'always')
table(dnt$number_of_bedroom, useNA = 'always')
table(dnt$number_of_baths, useNA = 'always')
table(dnt$zone, useNA = 'always')

# The observations that have no town name are 
# bad observations
d <- d[!is.na(d$town_name), ]


#' ## More Data Restriction with Further Restriction
#' 
#' In this section we primarily investigate `(1) Singe Family Properties` 
#' Note that the definition and the categorization
#' are not well defined and they are different by the town. Therefore,
#' they need to be examined thoroughly in order for us to make reasonable
#' decisions.

#' It appears that `state_use_description` is a finer category than the 
#' variable `state_use`.
length(unique(d$state_use_description))
length(unique(d$state_use))

# The following gives a comprehensive list of state use descriptions
sort(table(d$state_use_description, useNA = 'always'))

# Save the names
state_desc <- tolower(names(table(d$state_use_description, useNA = 'always')))

# Extract the single family homes using the regex exp
single_desc <- state_desc[grep("^(1 f|single|sf|one f|s f|sin|res single)", state_desc)]


# c for check
c <- d[which(tolower(d$state_use_description) %in% single_desc), ]
table(c$state_use, useNA = 'always')
sort(table(c$state_use, useNA = 'always'))
sort(table(c$state_use_description, useNA = 'always'))

#' Let's inspect further what single_desc does NOT indicate
#' 
#' WE NEED TO DEAL WITH `Condo | Condominium` and `Residential`
ce <- d[!(tolower(d$state_use_description) %in% single_desc), ]
sort(table(ce$state_use))
sort(table(ce$state_use_description))



#'
#'
#'
#'
#'
#'
#'
#'
#'




# Limit our focus to New Haven
dnh <- d[d$town_name == "NEW HAVEN", ]
sort(table(dnh$state_use_description, useNA = 'always'))
dnh$sf <- ifelse(tolower(dnh$state_use_description) %in% single_desc, 
                 "Single", "Others")

#' `APT` and `Condominium` need to be checked further
table(dnh$sf, dnh$state_use_description)

# Limit our focus to New Haven & Single Family
dnh_sf <- dnh[dnh$sf == "Single", ]
sort(table(dnh_sf$state_use_description))
sort(table(dnh_sf$state_use))



# View the distribution of single family based off these two codes
# Consider if other codes have Single Family homes
dgw <- d[d$town_name == "GREENWICH", ]
sort(table(dgw$state_use_description, useNA = 'always'))

# Limit our focus to New Haven & Single Family
dgw_sf <- dgw[tolower(dgw$state_use_description) %in% single_desc, ]
sort(table(dgw_sf$state_use_description))
sort(table(dgw_sf$state_use))


# Observe that code 100 has a lot of observations
# So this shows that even outside "101", "1010", and "1011"
# There are single family houses wihtin 
cc_often <- cc[cc$state_use %in% c("100", "105", "102", "1020", "110"), ]
sort(table(cc_often$state_use_description, useNA = 'always'))

# Residential types are quite often
cc_resi <- cc_often[cc_often$state_use_description == "Residential" 
                    & !is.na(cc_often$state_use_description), ]

nrow(cc_resi)

head(cc_resi)

table(cc_resi$state_use, useNA = 'always')


# Let's further focus our attention
d_gw <- d[d$town_name == "GREENWICH", ]
d_gw <- d_gw[d_gw$state_use %in% c("101", "1010", "1011", NA), ]


table(d$town_name, useNA = 'always')
sum(is.na(d$town_name))


d_gw_na <- d[is.na(d$state_use), ]
sort(table(d_gw_na$state_use_description, useNA = 'always'))



sort(table(d_gw$state_use_description, useNA = 'always'))
table(d_gw$condition_description,useNA = 'always')
table(d_gw$zone_description,useNA = 'always')


cc_gw <- cc[cc$town_name == "GREENWICH", ]
sort(table(cc_gw$state_use_description, useNA = 'always'))
nrow(cc_gw)




#' ## Exploratory Data Analysis
#' 
#' ### Assessed Total and Log-Transformation
#' (Comments: Log-transforming the `assessed_total` variable helps achieve 
#' normality and produces better model results.)

# Create subset of single family homes
# State use of 101 corresponds to single family homes -- state use description
# is unreliable (many different variations that have functionally same meaning)

# Prof Jay: Do more research on the `state_use` variable
# Look up other categorical variables to make the decision better
# We can think about focusing on New Haven or Greenwich
# table(d$assessed_total <= 0, d$town_name, useNA = 'always')

# Show JAY the previous codes and what has changed since
# if time allows

# We are manipulating the data such that it makes sense first.
# We are going to worry about the zones and types later on


# To check the dataframe
check_df <- d[which(d$state_use_description == "Single Family"), ]
table(check_df$state_use)

# The following categories capture the single family households
# of interests.
check_df_101 <- d[d$state_use == "101", ]
check_df_1010 <- d[d$state_use == "1010", ]
table(check_df_101$state_use_description)
table(check_df_1010$state_use_description)

# 1011 is not a suitable category for our purpose
# it's a different type
check_df_1011 <- d[d$state_use == "1011", ]
table(check_df_1011$state_use_description)

# View the distribution of single family based off these two codes
# Consider if other codes have Single Family homes
d$sh <- ifelse(is.na(d$state_use), "NA", 
               ifelse(d$state_use %in% c("101", "1010"), 
                      "Single", 
                      "Others"))
table(d$sh, d$town_name, useNA = 'always')


# Single family homes typically have a limited number of bedrooms
# We filter for 4 rooms and below to look through what other codes could be
# candidates for single family homes
c <- d[d$number_of_bedroom < 5, ]
cc <- d[!(c$state_use %in% c("101", "1010")), ]



cc_gw <- cc[cc$town_name == "GREENWICH", ]
sort(table(cc_gw$state_use_description))

cc_nh <- cc[cc$town_name == "NEW HAVEN", ]
sort(table(cc_nh$state_use_description))

sort(table(cc$state_use_description))
sort(table(cc_nh$state_use_description))
# c$state_use %in% c("100")
table(c[(c$state_use %in% c("100")), ]$state_use_description)
c[which(c$state_use_description == "Residential"), ]




# Let's look at the descriptions of all properties not captured by 101, 1010, 1011
unique(c[!(c$state_use %in% c("101", "1010", "1011")), ]$state_use_description)
# Interesting descriptions where we want to see the code: 
# Sin Fam WF, Single Fam, 1 Family Planned Comm, Single Fam M00, SFam BPW

#' Let's focus on the specific cities (New Haven, Greenwich) and do the above
#' analysis again. We might want to look more at what other features can help
#' us identify single family homes.

#table(c[!(c$state_use %in% c("101", "1010", "1011")), ]$state_use_description)





# Not Affluent Areas vs Affluent Areas
# STEP 1: Focus on (1) New Haven vs (2) Greenwich







h <- d[d$state_use == "101" | is.na(d$state_use), ]
table(h$state_use, useNA = 'always')
table(h$state_use, h$town_name, useNA = 'always')

# We need better startegies
table(d$state_use, d$town_name, useNA = 'always')




# This works: but let's do more



# Pre- Jay
# Filter out observations with less than 0 total values
# Comments: Use valuation year
# Comments: check Town dataset reliability
head(h[h$assessed_total <= 0 & !is.na(h$assessed_total), ]) 
nrow(h[h$assessed_total <= 0 & !is.na(h$assessed_total), ])

# Ask Prof Jay about this 
h <- h[!h$assessed_total <= 0 | is.na(h$assessed_total), ]

# There are many missing values.
# While I would hesitate to drop the missing values,
# this variable is important!
# in retrospect, this solves many other NA issues in other
# variables. 
sum(is.na(h$assessed_total))

# Verify the following decision by Prof. Jay
h <- h[!is.na(h$assessed_total), ]

# There are bad properties
# use these 'state_use_description' variables
range(h$assessed_total, na.rm = TRUE)
nrow(h[h$assessed_total < 10000 & !is.na(h$assessed_total), ])
nrow(h[h$assessed_total > 10000000 & !is.na(h$assessed_total), ])

# Reasonable filtering
h <- h[h$assessed_total > 10000 | is.na(h$assessed_total), ] 
h <- h[h$assessed_total < 100000000 | is.na(h$assessed_total), ]
# nrow(h[h$assessed_total > 100000000 | is.na(h$assessed_total), ])

# see the distribution
hist(h$assessed_total, main = "Histogram of Assessed Total")  # Original

# There are some bad data
# we are going to revisit this
# revisit: the unqualified emails
# recall depreciation
# We would like to have houses that are reasonable
nrow(h[h$assessed_total < 10000 & !is.na(h$assessed_total), ])
head(h[h$assessed_total < 10000 & !is.na(h$assessed_total), ])
tail(h[h$assessed_total < 10000 & !is.na(h$assessed_total), ])

# Take the log-transform
h$at_log <- log(h$assessed_total)

# current look
# Prof Jay comments: properties with more than $10M.
# Do these qualify as houses that we want to address? 
hist(h$at_log, main = "Histogram of Log-transformed Assessed Total")

#' ### Number of Rooms
#' Explore the distribution of room-related variables (bedrooms, bathrooms, 
#' half-baths, total rooms).

#' Check for NA values
#' While NA values make sense,
#' 0 total number of rooms do not make sense
nrow(h[is.na(h$total_rooms), ])  

# observer further
# check 0 number_of_bedrooms <- check more!
head(h[h$total_rooms == 0, ])
tail(h[h$total_rooms == 0, ])
range(h$total_rooms, na.rm = T)

# We are confident about these choices
h <- h[!(h$total_rooms == 0) | is.na(h$total_rooms), ]
h <- h[!(h$total_rooms > 400) | is.na(h$total_rooms), ]

# Ask Prof. Jay for the following decision
# h <- h[!(is.na(h$total_rooms)), ]

# The data looks much better
range(h$total_rooms, na.rm = T)

# Now let's inspect others
sum(is.na(h$total_rooms))
sum(is.na(h$number_of_bedroom))
sum(is.na(h$number_of_baths))
sum(is.na(h$number_of_half_baths))

# do we observe NA values in both columns?
# or just a single column? 
# This shows that we can solve many problmes
# by fixing one problem
temp <- h[!is.na(h$number_of_baths), ]
sum(is.na(temp$number_of_bedroom))

# Let's inspect
# Let's delay our decisions to drop the data yet
head(h[is.na(h$number_of_bedroom), ])
head(h[is.na(h$number_of_baths), ])

#' Let's see histograms
hist(h$total_rooms, main = "Histogram of Total Rooms")
hist(h$number_of_bedroom, main = "Histogram of Bedrooms")
hist(h$number_of_baths, main = "Histogram of Bathrooms")

#' Check if `total_rooms` equals the sum of other room variables (it doesn’t)
head(h[, c('total_rooms', 'number_of_bedroom', 
             'number_of_baths', 'number_of_half_baths')], 5)

#' ### Property Size
#' Check for missing values in `living_area` and `effective_area`
nrow(h[is.na(h$living_area), ])  # Missing values in living_area
nrow(h[is.na(h$effective_area), ])  # Missing values in effective_area

# Inspect
# Find town heterogeneites
head(h[is.na(h$effective_area), ])

# For some properties, living area is the same as
# effective area. For consistency, we can consider
# living area and disregard effective area and 
# partially obviate its NA problem
head(h[, c("living_area", "effective_area")])
tail(h[, c("living_area", "effective_area")])

#' ### Living Area

# Comments: Check town and living_area
# check town heterogeneity! 
# make sure that there can be white elephants
range(h$living_area, na.rm = T)
h <- h[h$living_area > 10 | is.na(h$living_area), ]
h <- h[h$living_area < 1000000 | is.na(h$living_area), ]

#' ### Effective Year Built (EYB)
#' Comments: binning is a great approach
#' Think about the DGP behind this: human errors
#' Explore the distribution of property age based on renovations and updates
table(h$eyb, useNA = 'always')
hist(h$ayb, main = "Histogram of Actual Year Built (AYB)")
hist(h$eyb, main = "Histogram of Effective Year Built (EYB)")
range(h$eyb, na.rm = T)  # eyb is not consistent at all
range(h$ayb, na.rm = T)  # ayb has fewer data problems

# Let's check the number of NA values in each var
sum(is.na(h$eyb))
sum(is.na(h$ayb))

# While it is true that `eyb` provides other information than `ayb`. 
# The lack of consistency across the dataset may introduce other biases
# that we may not want in our data. Therefore, we do not consider `eyb` 
# in our dataset.

#' Is it going to be worthwhile to drop `ayb`? It seems to tell us
#' important information! We need to think more
 
#' ### Zone Descriptions
#' Tabulate zone types and their descriptions to understand the zoning 
#' districts' effects on housing prices.
table(h$zone_description) 
table(h$zone)
sum(is.na(h$zone_description))  
sum(is.na(h$zone))  

# clean up the zone
h$zone <- gsub("\\.0$", "", h$zone)
table(h$zone)
length(table(h$zone))

#' ### Condition Description
#' 
#' Fit the basic models: Fit each basic model by the town name.
#' (pull the residuals for each level of condition) 
#' It will be an interesting exercise to normalize. 

# inspect more
table(h$condition_description)
length(table(h$condition_description))

# Convert condition descriptions to lowercase
h$condition_description <- tolower(h$condition_description)

# Clean up variations of 'average', 'good', 'very good', 'fair', 'poor', and other categories
h$condition_description <- gsub(
  "avarage|average-|average\\+|avg-good|av|averageerage|average / fair|average/fair|average-good", 
  "average", 
  h$condition_description
)

# Clean up variations of 'good'
h$condition_description <- gsub("good|g\\+|gd|good/very good", "good", 
                                  h$condition_description)

# Clean up variations of 'very good'
h$condition_description <- gsub("vgood|good-vg|vg\\+|very good|vg|very good/excellent|very good/good", 
                                  "very good",  
                                  h$condition_description)

# Clean up variations of 'fair'
h$condition_description <- gsub("^f$|fair|fair-avg|fair-average|fr|fair-averageg|fair/poor", 
                                  "fair", 
                                  h$condition_description)

# Clean up variations of 'poor'
h$condition_description <- gsub("poor|pr|poor / fair", "poor", 
                                  h$condition_description)

# Clean up 'unsound' and 'delapitated'
h$condition_description <- gsub("delapitated|unsound", "unsound", 
                                  h$condition_description)

# Clean up 'excellent'
h$condition_description <- gsub("excellent|ex|excellent/very good", "excellent", 
                                  h$condition_description)

# Clean up 'remodeled' and 'renovated'
h$condition_description <- gsub("remodeled|renovated", "renovated", 
                                  h$condition_description)


# check
table(h$condition_description)


# Additional Manipulations: Poor
h$condition_description <- ifelse(
  h$condition_description %in% c("ba"), 
  "poor", 
  h$condition_description
)


# Additional Manipulations: poor
h$condition_description <- ifelse(
  h$condition_description %in% c("fair / poor"), 
  "poor", 
  h$condition_description
)


# Additional Manipulations: fair
h$condition_description <- ifelse(
  h$condition_description %in% c("averageerage/ fair", "f+",
                                   "fair / averageerage"), 
  "fair", 
  h$condition_description
)

# Additional Manipulations: average
h$condition_description <- ifelse(
  h$condition_description %in% c("averageerage", "averageerage / good",
                                   "averageerage/good", "normal"), 
  "average", 
  h$condition_description
)

# Additional Manipulations: good
h$condition_description <- ifelse(
  h$condition_description %in% c("good / very good", "good+", "g-",
                                   "very good / good", "very good/good",
                                   "aa"), 
  "good", 
  h$condition_description
)

# Additional Manipulations: very good 
h$condition_description <- ifelse(
  h$condition_description %in% c("very good / excellent", "very good/excellent",
                                   "very good/excellentcel", " very good"), 
  "very good", 
  h$condition_description
)


# Very bad behaviors
head(h[h$condition_description == "r" & !is.na(h$condition_description), ])
head(h[h$condition_description == "rb"& !is.na(h$condition_description), ])
tail(h[h$condition_description == "rb"& !is.na(h$condition_description), ])
head(h[h$condition_description == "re"& !is.na(h$condition_description), ])
head(h[h$condition_description == "u" & !is.na(h$condition_description), ])
head(h[h$condition_description == "uc" & !is.na(h$condition_description), ])
head(h[h$condition_description == "unsound" & !is.na(h$condition_description), ])
head(h[h$condition_description == "renovated" & !is.na(h$condition_description) , ])
head(h[h$condition_description == "dilapidated"& !is.na(h$condition_description) , ])


# Remove any problematic values from analysis
h <- h[!(h$condition_description %in% c("r", "rb", "re", "u", "unsound",
                                              "uc", "dilapidated", "renovated")), ]

# check
table(h$condition_description)
head(h[h$condition_description == "0" & !is.na(h$condition_description), ], 5)
head(h[h$condition_description == "1" & !is.na(h$condition_description), ], 3)


# Making some arbitrary decisions
# was not able to find these data on the internet, unfortunately
# CHECK: Vision Appraisal [for the exact DGP]
h$condition_description[h$condition_description == "0"] <- "very poor"
h$condition_description[h$condition_description == "1"] <- "poor"
h$condition_description[h$condition_description == "2"] <- "fair"
h$condition_description[h$condition_description == "3"] <- "average"
h$condition_description[h$condition_description == "4"] <- "good"
h$condition_description[h$condition_description %in% c("5", "6")] <- "very good"
h$condition_description[h$condition_description %in% c("7", "8")] <- "excellent"

# Check the results
table(h$condition_description)

# Convert back to sentence case
h$condition_description <- tools::toTitleCase(h$condition_description)


# Convert condition_description to a factor variable with ordered levels
h$condition_description <- factor(h$condition_description, 
                                    levels = c("Very Poor", "Poor", "Fair", 
                                               "Average", "Good", 
                                               "Very Good", "Excellent"))

#' ## Final Check: Assessing and Understanding Data Quality

# Condition Description
sum(is.na(h$condition_description))
table(h$condition_description, useNA = "always")
head(h[is.na(h$condition_description), ])

# Number of Rooms
sum(is.na(h$total_rooms))
sum(is.na(h$number_of_bedroom))
sum(is.na(h$number_of_baths))
sum(is.na(h$number_of_halfbaths))

# Zone
sum(is.na(h$zone))

# Living Area Size
sum(is.na(h$living_area))

# Assessed Total
sum(is.na(h$assessed_total))


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


dr <- h[, vars_interests]  # Select relevant columns


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

          