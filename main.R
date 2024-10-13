#' ---
#' title: "S&DS 625: Housing Project"
#' date: "`r Sys.Date()`"
#' author: "Matt Shu, Peter Yun, Eugene Han"
#' output: pdf_document
#' urlcolor: blue
#' ---
#' 
#' # Introduction
#' 
#' In this study, we aim to explore how a home's proximity to food retailers,
#' such as grocery stores and supermarkets, contributes to its value. This is
#' a valuable question to answer from the perspective of someone trying to buy
#' a home; for instance, an individual may be interested in moving
#' with their family to an affordable home while being located within a
#' convenient distance from a grocery store to shop for food. For this case
#' study, we specifically focus on single-family homes in New Haven and
#' consider their total assessed values.
#' 
#' In addition to the parcel dataset for Connecticut properties, we utilize
#' the Connecticut Supplemental Nutrition Assistance Program (SNAP) Authorized 
#' Retailers [dataset](https://data.ct.gov/Health-and-Human-Services/Connecticut-Supplemental-Nutrition-Assistance-Prog/jb42-uhnf/about_data),
#' which contains information such as store type (supermarket, convenience 
#' store, farmer's market, etc.) and latitude/longitude coordinates that we
#' use together with the spatial format of the parcel data. 
#' 
#' 
#' # Data Preprocessing and Exploration
#' 
#' ## Subset Construction
#' 
#' In order to address our study's objective, we take the perspective of a
#' potential homebuyer and restrict our analysis to single-family homes. This
#' came with several challenges:
#' - State use codes that were clearly labeled as single-family homes according 
#' to their descriptions were not always reliable. There exist cases where
#' these "single-family" properties actually pointed to vacant lots or other
#' land used for non-residential purposes. Despite this, these codes were
#' generally effective as an initial filter.
#' - State use codes and descriptions were inconsistent across different towns.
#' - Data quality and completeness varied significantly across towns; for some
#' towns, initial filtering based on state use code would be more effective and
#' lead to less case-by-case inspections of state use descriptions.
#' 

# Load CT parcel data
d <- st_read("data/CT-parcel-data/4c5501b8-b68e-4888-bf6a-d92670d69c3b.gdb/")

# Observe below that state use descriptions are unreliable, but also that 
# multiple state use codes can correspond to single family homes
d$State_Use_Description <- toupper(d$State_Use_Description)

# Filter for codes that contain the word FAMILY and could be single-family
d_sf <- d[which(grepl("FAMILY", d$State_Use_Description) & 
                !grepl("[2-9]", d$State_Use_Description) & 
                !grepl("TWO", d$State_Use_Description) & 
                !grepl("THREE", d$State_Use_Description) & 
                !grepl("FOUR", d$State_Use_Description) & 
                !grepl("FIVE", d$State_Use_Description) & 
                !grepl("SIX", d$State_Use_Description) & 
                !grepl("MULTI", d$State_Use_Description)), ]

# Display table of mappings between State Use Description and State Use
table(d_sf$State_Use_Description, d_sf$State_Use, useNA = "always")

#'
#' We observe that the large majority of properties in this subset are captured
#' by state use codes "101" and "1010", with these mapping to multiple
#' descriptions (One Family, Res. Single Family, Single Family, Single Family
#' Res.) which all have the same meaning. As a result, we can use these codes
#' to form a heuristic to gauge town data quality with respect to how easily
#' we can isolate single-family homes.
#'

# View the distribution of single family homes with codes 101 and 1010 in each 
# town, looking also at NAs
d$sh <- ifelse(is.na(d$State_Use), NA, 
               ifelse(d$State_Use %in% c("101", "1010"), "Single", "Others"))

# Comment out for brevity
# table(d$sh, d$Town_Name, useNA = "always")

#' 
#' The distribution of NAs and how many single family homes are captured by the
#' two codes depends a lot on the town, although it's important to note that the 
#' percentage of single family homes in different Connecticut towns also varies.
#' 
#' Going forward, let's zoom into a particular town/city: New Haven. Based on
#' the table, 9168 properties have state use code "101" or "1010", 2968 are 
#' missing a state use code, and 15242 fall in the "Others" bin. Clearly, many
#' candidate single-family homes are captured by the two state use codes, but
#' there are also more than 18000 properties that we need to investigate
#' further to identify other single-family homes. In addition to requiring a 
#' closer look, New Haven is also interesting with respect to our study
#' because parts of the city are considered food deserts.
#' 

# Subset the original dataset to only New Haven
dnh <- d[d$Town_Name == "NEW HAVEN", ]

# Some initial observations show the many codes and descriptions, we comment
# this out to reduce clutter in the output
# table(dnh$State_Use)
# table(dnh$State_Use_Description)
# table(dnh$State_Use, dnh$State_Use_Description, useNA = "always")

#'
#' We repeat the same analysis above looking at the descriptions with FAMILY
#' and observe that in New Haven, descriptions containing FAMILY that do not 
#' refer to multi-family homes are all captured by description SINGLE FAMILY
#' and all of these codes are 1010.  
#' 

nh_sf <- dnh[which(grepl("FAMILY", dnh$State_Use_Description) & 
                   !grepl("[2-9]", dnh$State_Use_Description) & 
                   !grepl("TWO", dnh$State_Use_Description) & 
                   !grepl("THREE", dnh$State_Use_Description) & 
                   !grepl("FOUR", dnh$State_Use_Description) & 
                   !grepl("FIVE", dnh$State_Use_Description) & 
                   !grepl("SIX", dnh$State_Use_Description) & 
                   !grepl("MULTI", dnh$State_Use_Description)), ]
table(nh_sf$State_Use, nh_sf$State_Use_Description, useNA = "always")

#' 
#' Moreover, we also observe that 101x codes correspond to single-family homes, 
#' with the last digit indicating some type of accessory/special feature. As a
#' result, we can create an initial filter for candidate single-family homes
#' and focus on the remaining state use codes/descriptions.
#' 

# Create state use code-based filter checking if state use code starts with 101
sfh_coded <- dnh[which(startsWith(dnh$State_Use, "101")), ]
table(sfh_coded$State_Use, sfh_coded$State_Use_Description, useNA = "always")

#' 
#' While we were able to confidently skip over properties with certain state use
#' descriptions such as "APT 4-UNIT", "PARK GAR  MDL-96", and "YACHT CLUB", 
#' others weren't as clear. For instance, some descriptions are prefixed by 
#' "HSNG AUTH" or "CITY", indicating ownership by a housing authority or the 
#' city of New Haven, with "HSNG AUTH  MDL-01" and "CITY  MDL-01" corresponding 
#' to single-family homes based on manual inspection and cross-referencing with
#' websites like Zillow and Redfin. On the other hand, "MDL" suffixes other than
#' "MDL-01" did not correspond to single-family homes. As a result, we can
#' construct another loose filter for additional candidate single-family homes
#' based on the inclusion of "MDL-01" in the state use description column.
#' 

# Create state use description-based filter checking for inclusion of "MDL-01"
# Exclude apartments
mdl_01 <- dnh[which(grepl("MDL-01", dnh$State_Use_Description) & 
                    !grepl("APT", dnh$State_Use_Description)), ]

# Combine with state use code-based candidates
sfh <- rbind(sfh_coded, mdl_01)
dim(sfh)

#' 
#' It's important to note that our methods for isolating single-family homes, 
#' while motivated by meticulous exploration and cross-referencing properties,
#' is based on heuristics. In particular, our "MDL-01" filter may have
#' introduced some properties that shouldn't be considered single-family either
#' due to data quality issues or the fact that it wasn't practical to spot-check
#' every single property with state use description containing "MDL-01".
#' 
#' To address this, we will run a few sanity checks on several variables and
#' investigate extreme cases.
#' 

# Explore extremes of assessed total
head(st_drop_geometry(sfh[order(sfh$Assessed_Total, decreasing = T), 
                          c("Link", "Owner", "Location", "Assessed_Total", 
                            "State_Use_Description")]), 10)
tail(st_drop_geometry(sfh[order(sfh$Assessed_Total, decreasing = T), 
                          c("Link", "Owner", "Location", "Assessed_Total", 
                            "State_Use_Description")]), 10)

#' 
#' Examining the higher end of assessed total values, "358 SPRINGSIDE AV" stands
#' out with an assessed total of almost $16 million. This is unrealistic and is
#' confirmed to be a park/school area with 3 buildings; it appears that the
#' data regarding the entire plot of land has incorrectly been attributed to the
#' first of the three buildings, which indeed is a single-family home, based on
#' cross-referencing the Vision Appraisal database. Similarly,
#' "223 EAST GRAND AV" corresponds to a plot of land with a collection of 3 
#' buildings belonging to a religious society. Although not in the list of 
#' high assessed totals, we found through trial and error that "156 COVE ST"
#' suffered from the same issue of multiple buildings in the same area of land.
#' Therefore, these 3 properties shouldn't be considered in our subset of 
#' single-family homes.
#' 
#' The lower end of assessed total values, on the other hand, were confirmed
#' through Vision Appraisal and appears to be in part due to severe
#' depreciation based on their "effective year built" measures.
#' 

# Occupancy may give us a hint, expectation is 1 if all data is accurate
table(sfh$Occupancy, useNA = "always")

# Check 0 case - outbuilding at 115 CRANSTON ST, valid
st_drop_geometry(sfh[which(sfh$Occupancy == 0), c("Owner", "Location")])

# Check NA cases - all confirmed to be single-family
st_drop_geometry(sfh[is.na(sfh$Occupancy), c("Owner", "Location")])

# Check >= 4 cases, commented out to avoid long output
# Properties with a lot of occupants are generally rooming houses
# Multiple tenants, but the home itself is a single-family model according to
# Vision Appraisal
# st_drop_geometry(sfh[which(sfh$Occupancy >= 4), ])

#' 
#' We also checked for any duplicate listings and found that
#'


# Remove properties that aren't single-family and incorrect duplicate
to_exclude <- c("52070-392 1188 00101", "52070-094 0999 02600", 
                "52070-033 0868 00900", "52070-142 1060 01400")
sfh_final <- sfh[which(!(sfh$Link %in% to_exclude)), ]


#' 
#' ## Base Features
#' 
#' We consider the following variables:
#' 
#' 1. **Effective Area** - By accounting for different parts of a property
#'    (e.g. a garage or porch) in addition to living area, effective area
#'    better captures the contributory value of these property features than
#'    living area alone. We would expect that properties with larger effective
#'    areas tend to have higher total assessed values. 
#'    
#' 2. **Number of Bedrooms** - The number of bedrooms is related to the amount
#'    of living space in a home, which in turn we expect to be associated with 
#'    a home's assessed value.
#' 
#' 3. **Total Number of Bathrooms** - Like the number of bedrooms, the number
#'    of bathrooms is also related to the amount of living space in a home.
#'    To get a single total, we define Total Bathrooms as Number_of_Baths + 
#'    0.5 * Number_of_Half_Baths.
#' 
#' 4. **Condition** - Property condition categories (e.g., Excellent, Poor) 
#'    are expected to show variation in housing prices, with "worse" conditions
#'    being associated with lower assessed values.
#' 
#' 5. **Zone** - Different zoning regulations (e.g., residential, commercial) 
#'    that affect property use, including permissions for commercial activities 
#'    like restaurants, can influence assessed value.
#' 
#' 6. **Effective Age** - Effective year built (EYB), unlike actual year built
#'    (AYB), accounts for home improvements. We can create a variable for
#'    Effective Age = (2024 - EYB) which is more intuitive, where we expect
#'    an "older" home to have a lower assessed value from depreciation due to
#'    lack of renovations/modernization.
#' 
#' 
#' Since we're dealing with price data in our response, we will work with the
#' logarithm of assessed total value.
#' 

# Check for missing values
sum(is.na(sfh_final$Assessed_Total))

# Create Log_Assessed_Total variable and plot histogram as sanity check
# Histogram looks reasonable
sfh_final$Log_Assessed_Total <- log(sfh_final$Assessed_Total)
hist(sfh_final$Log_Assessed_Total)

#' 
#' Next, we'll take a look at Effective Area. When plotting vs. log assessed
#' total, we see that using a square root transformation results in a more
#' linear relationship in the scatterplot while still having a natural
#' interpretation.
#' 

# Check for missing values
sum(is.na(sfh_final$Effective_Area))

# Plot vs. Log Assessed Total, untransformed
plot(sfh_final$Effective_Area, sfh_final$Log_Assessed_Total)

# Has some curvature, try square root transformation
# More linear relationship
sfh_final$Sqrt_Effective_Area <- sqrt(sfh_final$Effective_Area)
plot(sfh_final$Sqrt_Effective_Area, sfh_final$Log_Assessed_Total)


#' 
#' We observe that there are a number of bedrooms that are
#' equal to 0 and has NA values. We inspect further to find out
#' what these properties are.
#' 
#' Since properties with 0 number of beds are small, we manually check
#' every property. After cross-checking with Vision Appraisal, it appears
#' that some missing values have been converted to 0. We fix the values for
#' these properties as we expect single-family homes to have at least 1
#' bedroom unless they are studios.
#'

table(sfh_final$Number_of_Bedroom, useNA = "always")

# 34 BURTON ST, 132 COVE ST - studio

#' 
#' ## Food Access Features
#' 

# Load SNAP data
s <- read.csv("data/CT_SNAP_Authorized_Retailers_20240920.csv")

# Convert to spatial
sg <- st_as_sf(s, coords = c("Longitude", "Latitude"), crs = 4326, remove=FALSE)



#' 
#' # Modeling
#' ## Baseline Model
#' 

# Fit baseline model
m1 <- lm(Log_Assessed_Total ~ Sqrt_Effective_Area + Number_of_Bedroom + 
         Total_Bathrooms + Condition + Zone_Grouped + Effective_Age, 
         data = sfh_final)
summary(m1)

# Diagnostic plots

#' 
#' ## Proposed Model
#' 

# Fit proposed model with food access features
m2 <- lm()

# Diagnostic plots

# Model comparison
anova(m1, m2)

#' 
#' # Conclusion
#' 