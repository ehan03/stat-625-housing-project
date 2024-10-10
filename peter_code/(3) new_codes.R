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
#' Here, we do not drop NA values. This is to ensure that we are making 
#' choices that we are sure about first to reduce complexities. 
#' 
#' 
#' ### Number of Bedrooms and Number of Bathrooms

# Number of Bedrooms: We want less than 5 number of bedrooms
nrow(d)
sum(d$number_of_bedroom > 5, na.rm = T)
d <- d[d$number_of_bedroom < 5 | is.na(d$number_of_bedroom), ]
head(d)


# Number of Bathrooms: We want at least one bathroom
# Checked 59 BECKWITH RD: does not look like a house
# Checked 98 NORTHROP RD: is not a house
# Checked 730 AMITY RD: this is a hoop house
head(d[d$number_of_baths == 0 | is.na(d$number_of_baths), ], 10)
sum(d$number_of_baths == 0, na.rm = T)
d <- d[d$number_of_baths > 0 | is.na(d$number_of_baths), ]
head(d)


#' ### Assessed Total

# Assessed Total: we do not want too cheap houses
# We want reasonably priced houses
nrow(d)
sum(d$assessed_total < 50000, na.rm = T)
d <- d[d$assessed_total > 50000, ]
head(d)

# Assessed Total: we do not want too expensive houses
# We do not want too expensive houses 
nrow(d)
sum(d$assessed_total > 10000000, na.rm = T)
d <- d[d$assessed_total < 10000000, ]
head(d)

#' ### Living Area
#' 


# Living Area: Living Area has to be greater than 0
# Checked 43 EAST SHORE DR: This looks like a garage
# Checked 196 HIDDEN LAKE RD: This definitely is not a house 
head(d[d$living_area == 0 & !is.na(d$living_area), ])
sum(d$living_area == 0, na.rm = T)
d <- d[d$living_area != 0 | is.na(d$living_area), ]
head(d)

# Living Area: Living Area has to be greater than 200
# We do not want to live in areas that have less
# than 200 square feet
sum(d$living_area < 200, na.rm = T)
d <- d[d$living_area > 200 | is.na(d$living_area), ]
head(d)


#' ### Town Name
#' We have previously shown that the DGP behind the data are heterogeneous
#' by town among other things. We select on towns based on our observation
#' that a great portion of our observations are not good. Therefore,
#' we remove these observations from our data frame. 


# We are going to inspect what the data looks like for those
# that have missing town names
nrow(d)
dnt <- d[is.na(d$town_name), ]
nrow(dnt)

# The following shows that they are bad observations
# that have no real data.
table(dnt$assessed_total, useNA = 'always')
table(dnt$number_of_bedroom, useNA = 'always')
table(dnt$number_of_baths, useNA = 'always')
table(dnt$zone, useNA = 'always')

# Therefore, we remove them from our data
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

# Save the names of single family households
state_desc <- tolower(names(table(d$state_use_description, useNA = 'always')))

# Extract the single family homes using the regex exp
single_desc <- state_desc[grep("^(1 f|single|sf|one f|s f|sin|res single)", 
                               state_desc)]


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


#' The following inspects homes classified as Residential and Condominium.
#' They are great properties that we, as potential homebuyers, will 
#' seriously consider.
#' 
#' We have come to the conclusion that `Residential` and `Condominiums`
#' should be considered as single family homes. This is especially true
#' since we have already made precise sample restriction where we 
#' limited the number of rooms, the number of bathrooms, the assesed total,
#' and other things.
#' 
#' At the end of the day, we are exploring what might be good for potential
#' home buyers. The classificaiton is v

# Check Residential

# check 54 ROCKLEDGE DR: good
# check 90 REMINGTON RD: good
# check 956 RIDGE RD: good
# check 38 BIRCH ROAD: good
# check 38 WEST MOUNTAIN ROAD: good
ce_res <- ce[ce$state_use_description %in% c("Residential", "Res Dwelling"), ]
ce_res[sample(nrow(ce_res), size = 5), ]
res_desc <- state_desc[grep("^res(?!t)", state_desc, perl = TRUE)]

# Yes, they are legitmate properties
tail(d[tolower(d$state_use_description) %in% res_desc, ])


# Check Condominium

# check 15 BRIGHTON PARK WAY: good
# check 670 QUINNIPIAC AV: good
# 319 NEW BRITAIN RD  #104: good 
# 33 WHITNEY LN : good
# 319 NEW BRITAIN RD #203: good 
ce_cond <- ce[ce$state_use_description %in% c("Condominium"), ]
ce_cond[sample(nrow(ce_cond), size = 5), ]
condo_desc <- state_desc[grep("^(condo)", state_desc, perl = TRUE)]

# Yes, they are legitimate properties
tail(d[tolower(d$state_use_description) %in% condo_desc, ])

# Create the sf variable that indicates a single family if the description
# falls into `single_desc`, `res_desc`, `condo_desc` defined above
d$sf <- ifelse(tolower(d$state_use_description) %in% c(single_desc, res_desc, condo_desc), 1, 0)

# Let's check if this works great 
table(d$sf, d$town_name)

# We have found many towns that have no single family properties.
# This does not make sense. We inspect further
vec_true <- as.vector(table(d$sf, d$town_name)[2, ] == 0)
# Save unusual towns
uts <- colnames(table(d$sf, d$town_name))[vec_true]


# The codes below show that for these towns, they have no state use description
# While it would be ideal if we can use the state_use code, we have no
# documentation to guide our decisions
duts <- d[d$town_name %in% uts, ]
table(duts$state_use_description, useNA = 'always')
sort(table(duts$state_use, useNA = 'always'))

# We observe that 100, 101, 200, 107, 600 are the top 5 most frequent codes
# Let's see the cross over
dsf_t <- d[d$sf == 1 & d$state_use %in% c("100", "101", "200", "107", "600"), ]

# 101, 100, and 107 match
unique(dsf_t$state_use)

# We find out that
# 100 refers to residential
# 101 and 107 refers to single family
table(dsf_t$state_use, dsf_t$state_use_description)

#
d$state_use_description <- ifelse(is.na(d$state_use_description) & 
                                    d$state_use == "100", 
                                  "Residential", d$state_use_description)

d$state_use_description <- ifelse(is.na(d$state_use_description) & 
                                    d$state_use %in% c("101", "107"), 
                                  "Single Family", d$state_use_description)


# calling the function again 
d$sf <- ifelse(tolower(d$state_use_description) %in% c(single_desc, res_desc, condo_desc), 1, 0)


# Let's check again if this works great 
table(d$sf, d$town_name, useNA = 'always') 


# We cannot evaluate whether SHERMAN, AVON, CORNWALL has single family homes
# Perhaps we can select 90% at random and assign single family homes

imp_towns <- c("SHERMAN", "AVON", "BETHEL", "DERBY", "CORNWALL")
table(d[d$town_name %in% imp_towns, ]$state_use, useNA = 'always')
table(d[d$town_name %in% imp_towns, ]$state_use_description, useNA = 'always')

# While we considered using zone and zone descriptions to advance 
# our analysis. We learned that a property falling in a residential zone 
# does not necessarily mean that a property must be a residential property
# Without detailed knowledge, we cannot proceed further without making
# further assumptions. 
sort(table(d[d$town_name %in% imp_towns, ]$zone, useNA = 'always'))
table(d[d$town_name %in% imp_towns, ]$zone_description, useNA = 'always')


# Hence, we drop thess towns from analysis
dsf <- d[d$sf == 1 & !(d$town_name %in% imp_towns), ]

#' ## Exploratory Data Analysis
#' 
#' ### Assessed Total and Log-Transformation
#' (Comments: Log-transforming the `assessed_total` variable helps achieve 
#' normality and produces better model results.)

# see the distribution
hist(dsf$assessed_total, main = "Histogram of Assessed Total")  # Original
dsf$at_log <- log(dsf$assessed_total)
hist(dsf$at_log, main = "Histogram of Logged Assessed Total")  # Original



#' ### Number of Rooms
#' Explore the distribution of room-related variables (bedrooms, bathrooms, 
#' half-baths, total rooms).

#' 0 total number of rooms do not make sense
nrow(dsf)
sum(dsf$total_rooms == 0, na.rm = T)

# observer further
# check 0 number_of_bedrooms <- check more!
range(dsf$total_rooms, na.rm = T)

# We are confident about these choices
dsf <- dsf[!(dsf$total_rooms == 0) | is.na(dsf$total_rooms), ]
sum(dsf$total_rooms > 15, na.rm = T)
dsf <- dsf[!(dsf$total_rooms > 15) | is.na(dsf$total_rooms), ]

# The data looks much better
range(dsf$total_rooms, na.rm = T)

# Now let's inspect others
sum(is.na(dsf$total_rooms))
sum(is.na(dsf$number_of_bedroom))
sum(is.na(dsf$number_of_baths))
sum(is.na(dsf$number_of_half_baths))

# do we observe NA values in both columns?
# or just a single column? 
# This shows that we can solve many problems
# by fixing one problem
temp <- dsf[!is.na(dsf$number_of_baths), ]
sum(is.na(temp$number_of_bedroom))

# Let's inspect
# Let's delay our decisions to drop the data yet
head(dsf[is.na(dsf$number_of_bedroom), ])
head(dsf[is.na(dsf$number_of_baths), ])

#' Let's see histograms
hist(dsf$total_rooms, main = "Histogram of Total Rooms")
hist(dsf$number_of_bedroom, main = "Histogram of Bedrooms")
hist(dsf$number_of_baths, main = "Histogram of Bathrooms")

#' Check if `total_rooms` equals the sum of other room variables (it doesn’t)
head(dsf[, c('total_rooms', 'number_of_bedroom', 
             'number_of_baths', 'number_of_half_baths')], 5)

#' ### Property Size
#' Check for missing values in `living_area` and `effective_area`
nrow(dsf[is.na(dsf$living_area), ])  # Missing values in living_area
nrow(dsf[is.na(dsf$effective_area), ])  # Missing values in effective_area

# For some properties, living area is the same as
# effective area. For consistency, we can consider
# living area and disregard effective area and 
# partially obviate its NA problem
head(dsf[, c("living_area", "effective_area")])
tail(dsf[, c("living_area", "effective_area")])

#' ### Living Area

# Comments: Check town and living_area
# check town heterogeneity! 
# make sure that there can be white elephants
range(dsf$living_area, na.rm = T)
sum(dsf$living_area > 7000, na.rm = T)

dsf <- dsf[dsf$living_area > 200 | is.na(dsf$living_area), ]
dsf <- dsf[dsf$living_area < 7000 | is.na(dsf$living_area), ]

#' ### Effective Year Built (EYB)
#' Comments: binning is a great approach
#' Think about the DGP behind this: human errors
#' Explore the distribution of property age based on renovations and updates
table(dsf$ayb, useNA = 'always')
table(dsf$eyb, useNA = 'always')
hist(dsf$ayb, main = "Histogram of Actual Year Built (AYB)")
hist(dsf$eyb, main = "Histogram of Effective Year Built (EYB)")
range(dsf$eyb, na.rm = T)  # eyb is not consistent at all
range(dsf$ayb, na.rm = T)  # ayb has fewer data problems

# Let's check the number of NA values in each var
sum(is.na(dsf$eyb))
sum(is.na(dsf$ayb))

# While it is true that `eyb` provides other information than `ayb`. 
# The lack of consistency across the dataset may introduce other biases
# that we may not want in our data. Therefore, we do not consider `eyb` 
# in our dataset.

#' Is it going to be worthwhile to drop `ayb`? It seems to tell us
#' important information! We need to think more
 
#' ### Zone Descriptions
#' Tabulate zone types and their descriptions to understand the zoning 
#' districts' effects on housing prices.
table(dsf$zone_description) 
table(dsf$zone)
sum(is.na(dsf$zone_description))  
sum(is.na(dsf$zone))  

# clean up the zone
dsf$zone <- gsub("\\.0$", "", dsf$zone)
table(dsf$zone)
length(table(dsf$zone))

#' ### Condition Description
#' 
#' Fit the basic models: Fit each basic model by the town name.
#' (pull the residuals for each level of condition) 
#' It will be an interesting exercise to normalize. 

# inspect more
table(dsf$condition_description)
length(table(dsf$condition_description))

# Convert condition descriptions to lowercase
dsf$condition_description <- tolower(dsf$condition_description)

# Clean up variations of 'average', 'good', 'very good', 'fair', 'poor', and other categories
dsf$condition_description <- gsub(
  "avarage|average-|average\\+|avg-good|av|averageerage|average / fair|average/fair|average-good", 
  "average", 
  dsf$condition_description
)

# Clean up variations of 'good'
dsf$condition_description <- gsub("good|g\\+|gd|good/very good", "good", 
                                  dsf$condition_description)

# Clean up variations of 'very good'
dsf$condition_description <- gsub("vgood|good-vg|vg\\+|very good|vg|very good/excellent|very good/good", 
                                  "very good",  
                                  dsf$condition_description)

# Clean up variations of 'fair'
dsf$condition_description <- gsub("^f$|fair|fair-avg|fair-average|fr|fair-averageg|fair/poor", 
                                  "fair", 
                                  dsf$condition_description)

# Clean up variations of 'poor'
dsf$condition_description <- gsub("poor|pr|poor / fair", "poor", 
                                  dsf$condition_description)

# Clean up 'unsound' and 'delapitated'
dsf$condition_description <- gsub("delapitated|unsound", "unsound", 
                                  dsf$condition_description)

# Clean up 'excellent'
dsf$condition_description <- gsub("excellent|ex|excellent/very good", "excellent", 
                                  dsf$condition_description)

# Clean up 'remodeled' and 'renovated'
dsf$condition_description <- gsub("remodeled|renovated", "renovated", 
                                  dsf$condition_description)


# check
table(dsf$condition_description)


# Additional Manipulations: Poor
dsf$condition_description <- ifelse(
  dsf$condition_description %in% c("ba"), 
  "poor", 
  dsf$condition_description
)


# Additional Manipulations: poor
dsf$condition_description <- ifelse(
  dsf$condition_description %in% c("fair / poor"), 
  "poor", 
  dsf$condition_description
)


# Additional Manipulations: fair
dsf$condition_description <- ifelse(
  dsf$condition_description %in% c("averageerage/ fair", "f+",
                                   "fair / averageerage"), 
  "fair", 
  dsf$condition_description
)

# Additional Manipulations: average
dsf$condition_description <- ifelse(
  dsf$condition_description %in% c("averageerage", "averageerage / good",
                                   "averageerage/good", "normal"), 
  "average", 
  dsf$condition_description
)

# Additional Manipulations: good
dsf$condition_description <- ifelse(
  dsf$condition_description %in% c("good / very good", "good+", "g-",
                                   "very good / good", "very good/good",
                                   "aa"), 
  "good", 
  dsf$condition_description
)

# Additional Manipulations: very good 
dsf$condition_description <- ifelse(
  dsf$condition_description %in% c("very good / excellent", "very good/excellent",
                                   "very good/excellentcel", " very good"), 
  "very good", 
  dsf$condition_description
)


# Very bad behaviors
head(dsf[dsf$condition_description == "r" & !is.na(dsf$condition_description), ])
head(dsf[dsf$condition_description == "rb"& !is.na(dsf$condition_description), ])
tail(dsf[dsf$condition_description == "rb"& !is.na(dsf$condition_description), ])
head(dsf[dsf$condition_description == "re"& !is.na(dsf$condition_description), ])
head(dsf[dsf$condition_description == "u" & !is.na(dsf$condition_description), ])
head(dsf[dsf$condition_description == "uc" & !is.na(dsf$condition_description), ])
head(dsf[dsf$condition_description == "unsound" & !is.na(dsf$condition_description), ])
head(dsf[dsf$condition_description == "renovated" & !is.na(dsf$condition_description) , ])
head(dsf[dsf$condition_description == "dilapidated"& !is.na(dsf$condition_description) , ])


# check
table(dsf$condition_description)
head(dsf[dsf$condition_description == "0" & !is.na(dsf$condition_description), ], 5)
head(dsf[dsf$condition_description == "1" & !is.na(dsf$condition_description), ], 3)


# Making some arbitrary decisions
# was not able to find these data on the internet, unfortunately
# CHECK: Vision Appraisal [for the exact DGP]
dsf$condition_description[dsf$condition_description == "0"] <- "very poor"
dsf$condition_description[dsf$condition_description == "1"] <- "poor"
dsf$condition_description[dsf$condition_description == "2"] <- "fair"
dsf$condition_description[dsf$condition_description == "3"] <- "average"
dsf$condition_description[dsf$condition_description == "4"] <- "good"
dsf$condition_description[dsf$condition_description %in% c("5", "6")] <- "very good"
dsf$condition_description[dsf$condition_description %in% c("7", "8")] <- "excellent"

# Check the results
table(dsf$condition_description)

#
unvalid <- c("r", "rb", "re", "renovated", "unsound")
dsf <- dsf[!(dsf$condition_description %in% unvalid), ]

# Convert back to sentence case
dsf$condition_description <- tools::toTitleCase(dsf$condition_description)
unique(dsf$condition_description)

# Convert condition_description to a factor variable with ordered levels
dsf$condition_description <- factor(dsf$condition_description, 
                                    levels = c("Very Poor", "Poor", "Fair", 
                                               "Average", "Good", 
                                               "Very Good", "Excellent"))

#' ## Final Check: Assessing and Understanding Data Quality
unique(dsf$condition_description)

# Condition Description
sum(is.na(dsf$condition_description))
table(dsf$condition_description, useNA = "always")
head(dsf[is.na(dsf$condition_description), ])

# Number of Rooms
sum(is.na(dsf$total_rooms))
sum(is.na(dsf$number_of_bedroom))
sum(is.na(dsf$number_of_baths))
sum(is.na(dsf$number_of_halfbaths))

# Zone
sum(is.na(dsf$zone))

# Living Area Size
sum(is.na(dsf$living_area))

# Assessed Total
sum(is.na(dsf$assessed_total))


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


dr <- dsf[, vars_interests]  # Select relevant columns


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

          