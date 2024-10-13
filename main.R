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

# Load libraries
library(sf)
library(leaflet)

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
                            "State_Use_Description")]), 12)
tail(st_drop_geometry(sfh[order(sfh$Assessed_Total, decreasing = T), 
                          c("Link", "Owner", "Location", "Assessed_Total", 
                            "State_Use_Description")]), 12)

#' 
#' Examining the higher end of assessed total values, "358 SPRINGSIDE AV" stands
#' out with an assessed total of almost $16 million. This is unrealistic and is
#' confirmed to be a park/school area with 3 buildings; it appears that the
#' data regarding the entire plot of land has incorrectly been attributed to the
#' first of the three buildings, which indeed is a single-family home, based on
#' cross-referencing the Vision Appraisal database. Similarly,
#' "223 EAST GRAND AV" corresponds to a plot of land with a collection of 3 
#' buildings belonging to a religious society. Therefore, these 2 properties 
#' shouldn't be considered in our subset of single-family homes.
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
#' We also checked for any duplicate listings and found that there were two
#' properties at each of "1 RESERVOIR ST" and "115 CRANSTON ST". When
#' cross-referenced with Vision Appraisal, we found that the two properties
#' located at "1 RESERVOIR ST" correspond to two separate single-family homes.
#' In contrast, the two properties at "115 CRANSTON ST" refer to the same home,
#' based on the photos provided by Vision Appraisal, with one of the entries
#' containing incorrect information (0s for all room-related columns) which
#' should be removed.
#'

# List locations with more than one property
sfh[which(duplicated(sfh$Location)), ]$Location

# Inspect corresponding properties, commented out for brevity
# sfh[which(sfh$Location %in% c("1 RESERVOIR ST", "115 CRANSTON ST")), ]

#'
#' As a sanity check, we also plotted the properties superimposed on a map of
#' New Haven leveraging their shapefiles. We then inspected any properties that
#' were unusually large and checked Vision Appraisal, revealing several more
#' areas of land with multiple buildings' assessments represented by only one of
#' the properties.
#'

# Convert to latitude/longitude
sfh <- st_transform(sfh, crs = 4326)

# Map plot
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Center on New Haven
  setView(lng = -72.93, lat = 41.30, zoom = 12) %>%
  
  # Add sfh homes
  addPolygons(data = sfh,
              color = "black",
              weight = 1,
              fillColor = "black",
              popup = ~paste("Location:", Location,
                             "<br>State Use: ", State_Use_Description),
              group = "Homes")


# Remove properties that aren't single-family and incorrect duplicate
to_exclude <- c("52070-392 1188 00101", "52070-094 0999 02600", 
                "52070-033 0868 00900", "52070-102 1030 00100", 
                "52070-280 0249 02701", "52070-142 1060 01400", 
                "52070-125 1039 01341", "52070-098 1001 00200",
                "52070-024 0920 02800")
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
#' Since properties with 0 number of bedrooms are small, we manually check
#' every property. After cross-checking with Vision Appraisal, it appears
#' that some missing values have been converted to 0. We fix the values for
#' these properties as we expect single-family homes to have at least 1
#' bedroom unless they are studios such as the properties located at
#' "34 BURTON ST" and "132 COVE ST", using sites like Zillow, Redfin, and 
#' Realtor as references.
#'

# Get table of bedroom counts
table(sfh_final$Number_of_Bedroom, useNA = "always")

# Identify properties with 0 or missing number of bedrooms
st_drop_geometry(sfh_final[which(sfh_final$Number_of_Bedroom == 0 |
                                 is.na(sfh_final$Number_of_Bedroom)), 
                           c("Owner", "Location", "Number_of_Bedroom",
                             "Number_of_Baths", "Number_of_Half_Baths")])
bedroom_rows <- row.names(sfh_final[which(sfh_final$Number_of_Bedroom == 0 |
                                    is.na(sfh_final$Number_of_Bedroom)), ])
corrected_bedrooms <- c(0, 0, 1, 3, 6, 4)
sfh_final[bedroom_rows, "Number_of_Bedroom"] <- corrected_bedrooms

# Plot vs. log assessed total
plot(sfh_final$Number_of_Bedroom, sfh_final$Log_Assessed_Total)


#'
#' We repeat the same procedure for the number of bathrooms and half bathrooms.
#' Since there are more than 4000 properties listed with 0 half bathrooms, we
#' will focus on missing values since it would be impractical to check for
#' true 0 half bathrooms and distinguish them from artifacts of missingness.
#'

# Get table of bathroom counts
table(sfh_final$Number_of_Baths, useNA = "always")

# Identify properties with 0 or missing number of bathrooms
st_drop_geometry(sfh_final[which(sfh_final$Number_of_Baths == 0 |
                                   is.na(sfh_final$Number_of_Baths)), 
                           c("Owner", "Location", "Number_of_Bedroom", 
                             "Number_of_Baths", "Number_of_Half_Baths")])
bath_rows <- row.names(sfh_final[which(sfh_final$Number_of_Baths == 0 |
                                       is.na(sfh_final$Number_of_Baths)), ])

# 1395 CHAPEL ST does not have any information about number of bathrooms
# Replace 0 with NA to reflect missingness
corrected_baths <- c(2, 0, 2, 1, 2, 2, NA)
sfh_final[bath_rows, "Number_of_Baths"] <- corrected_baths

# Get table of half bathroom counts
table(sfh_final$Number_of_Half_Baths, useNA = "always")

# Identify properties with missing half bathroom counts
# Show first few rows for brevity
head(st_drop_geometry(sfh_final[which(is.na(sfh_final$Number_of_Half_Baths)), 
                           c("Owner", "Location", "Number_of_Bedroom", 
                             "Number_of_Baths", "Number_of_Half_Baths")]))
half_bath_rows <- row.names(sfh_final[which(is.na(
                                      sfh_final$Number_of_Half_Baths)), ])
corrected_half_baths <- c(1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 
                          0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1,
                          1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0)
sfh_final[half_bath_rows, "Number_of_Half_Baths"] <- corrected_half_baths

# Create total bathrooms variable
sfh_final$Number_of_Baths_Total <- sfh_final$Number_of_Baths + 
                                   0.5 * sfh_final$Number_of_Half_Baths

# Plot vs. log assessed total
plot(sfh_final$Number_of_Baths_Total, sfh_final$Log_Assessed_Total)


#'
#' Next, we inspect property condition, observing there is a nice 1-to-1
#' correspondence between conditions and their descriptions.
#'

# Table of condition and descriptions
table(sfh_final$Condition, sfh_final$Condition_Description, useNA = "always")

#' 
#' There are two properties with condition listed as "U". Inspecting these
#' locations in Vision Appraisal, it's likely that "U" represents Unknown or 
#' Unspecified as the condition data field is missing on Vision Appraisal.
#' Using Redfin, we replace these unknown conditions.
#' 

# Substitute with Redfin's condition data
unk_cond_rows <- row.names(sfh_final[which(sfh_final$Condition == "U"), ])
sfh_final[unk_cond_rows, "Condition"] <- c("A", "A")
sfh_final[unk_cond_rows, "Condition_Description"] <- c("Average", "Average")

#' 
#' We also see there is a condition listed as "F" with description "F". We
#' initially thought this may correspond to "Fair", and so to justify this we
#' made boxplots grouped by condition. We should expect the mean log assessed
#' total to increase with "better" condition levels.
#' 

# Boxplot of log assessed total by condition
boxplot(Log_Assessed_Total ~ factor(Condition, 
                                    levels = c("VP", "P", "F", "A", "G", 
                                               "VG", "E")), 
        xlab = "Condition", data = sfh_final)

# Update description to fair
sfh_final[which(sfh_final$Condition == "F"), "Condition_Description"] <- "Fair"

# We'll use condition description so that our model summaries later are easier
# to read and interpret
# Convert condition description to a factor and set "Average" as the reference
sfh_final$Condition_Description <- as.factor(sfh_final$Condition_Description)
sfh_final$Condition_Description <- relevel(sfh_final$Condition_Description,
                                           ref = "Average")

#' 
#' Next, we take a closer look at zoning districts. Because there are so many
#' different zones, with many only corresponding to a handful of homes, we
#' decided to group them based on their descriptions into five broader
#' categories that represent their overall use: Residential, Mixed-Use,
#' Commercial, Industrial, and Planned Development. The result is a more
#' interpretable categorical variable. To achieve this, we referred
#' to Article II of New Haven's Zoning Ordinances available on the
#' [Municode Library](https://library.municode.com/ct/new_haven/codes/zoning?nodeId=ZOOR_ARIIESDIZOMA).
#' 

# Table of zone counts
table(sfh_final$Zone)

# Define broader groupings based on zone descriptions
residential <- c("RS1", "RS2", "RS1/RS2", "RM1", "RM2", "RM2/RS2", "RH1", "RH2")
mixed_use <- c("RO", "BA/RM1", "BA/RM2", "BA/RS2", "CEM")
commercial <- c("BA", "BA1", "BB", "BC", "BD", "BD1")
industrial <- c("IL", "IH", "IH/RM2")
planned_development <- c("PDD 119", "PDD 26", "PDD 39", "PDD 49", "PDD 52", 
                         "PDU 102", "PDU 106", "PDU 108", "PDU 16", "PDU 72", 
                         "PDU 75", "PDU 95")

# Assign zone categories
sfh_final$Zone_Category <- ifelse(sfh_final$Zone %in% residential, 
                                  "Residential", NA)
sfh_final$Zone_Category <- ifelse(sfh_final$Zone %in% mixed_use, 
                                  "Mixed-Use", sfh_final$Zone_Category)
sfh_final$Zone_Category <- ifelse(sfh_final$Zone %in% commercial, 
                                  "Commercial", sfh_final$Zone_Category)
sfh_final$Zone_Category <- ifelse(sfh_final$Zone %in% industrial, 
                                  "Industrial", sfh_final$Zone_Category)
sfh_final$Zone_Category <- ifelse(sfh_final$Zone %in% planned_development, 
                                  "Planned", 
                                  sfh_final$Zone_Category)

# Convert to factor, set "Residential" as reference
sfh_final$Zone_Category <- as.factor(sfh_final$Zone_Category)
sfh_final$Zone_Category <- relevel(sfh_final$Zone_Category, ref = "Residential")

# Boxplot of log assessed total by zone category
boxplot(Log_Assessed_Total ~ Zone_Category, data = sfh_final, cex.axis = 0.8, 
        las = 2, xlab = "")

#'
#' Lastly, we create the effective age variable.
#'

# Check for missing effective year built
sum(is.na(sfh_final$EYB))

# Create effective age variable and plot against log assessed total
sfh_final$Effective_Age <- 2024 - sfh_final$EYB
plot(sfh_final$Effective_Age, sfh_final$Log_Assessed_Total)


#' 
#' ## Food Access Features
#' 
#' To create meaningful and interpretable features related to proximity to food,
#' we took two approaches to the idea of proximity:
#' 
#' 1. Distance to nearest food retailers
#' 2. Number of food retailers within some radius
#' 

# Load SNAP data
s <- read.csv("data/CT_SNAP_Authorized_Retailers_20240920.csv")

# Convert to spatial
sg <- st_as_sf(s, coords = c("Longitude", "Latitude"), crs = 4326, remove=FALSE)
sfh_final <- st_transform(sfh_final, crs = 4326)

# Visualize store locations, superimposed on New Haven map and geometries of
# single-family homes in our subset
pal <- colorFactor(
  palette = c("red", "blue", "green", "orange", "purple", "cyan", "brown"),
  domain = sg$Store.Type
)
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Center on New Haven
  setView(lng = -72.93, lat = 41.32, zoom = 13) %>%
  
  # Add sfh homes
  addPolygons(data = sfh_final,
              color = "black",
              weight = 1,
              fillColor = "transparent",
              popup = ~paste("Location:", Location,
                             "<br>State Use: ", State_Use_Description),
              group = "Homes") %>%
  
  # Add stores, colored by type
  addCircleMarkers(data = sg,
                   radius = 3,
                   color = ~pal(Store.Type),
                   stroke = TRUE,
                   weight = 1,
                   fillOpacity = 0.8,
                   popup = ~paste0("Store: ", Store.Name, 
                                   "<br>Type: ", Store.Type),
                   group = "Stores") %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = sg$Store.Type,
    title = "Store Types",
    opacity = 1
  )

#'
#' Certain store types like specialty stores are relatively infrequent.
#' Moreover, we could technically consider super stores and supermarkets
#' together, since we can view super stores as extensions of supermarkets that
#' happen to include non-food items as well. As a result, we decided to group
#' super stores and supermarkets together, leave convenience stores and
#' grocery stores as is as separate groups, and bucket the rest into "Other."
#' 

# View counts by store type
table(sg$Store.Type)

# Create store type groups
sg$Store.Type.Grouped <- "Other"
sg$Store.Type.Grouped[sg$Store.Type == "Convenience Store"] <- "Convenience Store"
sg$Store.Type.Grouped[sg$Store.Type == "Grocery Store"] <- "Grocery Store"
sg$Store.Type.Grouped[sg$Store.Type %in% 
                      c("Supermarket", "Super Store")] <- "Supermarket_store"

# Re-visualize with new groups
palGrouped <- colorFactor(
  palette = c("red", "green", "orange", "cyan"),
  domain = sg$Store.Type.Grouped
)
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Center on New Haven
  setView(lng = -72.93, lat = 41.32, zoom = 13) %>%
  
  # Add sfh homes
  addPolygons(data = sfh_final,
              color = "black",
              weight = 1,
              fillColor = "black",
              popup = ~paste("Location:", Location,
                             "<br>State Use: ", State_Use_Description),
              group = "Homes") %>%
  
  # Add stores, colored by type
  addCircleMarkers(data = sg,
                   radius = 3,
                   color = ~palGrouped(Store.Type.Grouped),
                   stroke = TRUE,
                   weight = 1,
                   fillOpacity = 0.8,
                   popup = ~paste0("Store: ", Store.Name, 
                                   "<br>Type: ", Store.Type),
                   group = "Stores") %>%
  addLegend(
    position = "bottomright",
    pal = palGrouped,
    values = sg$Store.Type.Grouped,
    title = "Store Types",
    opacity = 1
  )


#'
#' For our first approach to "proximity", we compute the distance in meters
#' to the nearest store of each grouped store type we defined earlier.
#'

# Transform back to WGS 84 Mercator so calculations are in meters
sfh_final <- st_transform(sfh_final, crs = 3857)
sg <- st_transform(sg, crs = 3857)

# Define the store types to analyze
target_store_types <- unique(sg$Store.Type.Grouped)

# Initialize distance and area columns for each target store type
for (type in target_store_types) {
  
  # Define column names for distance and area
  dist_col <- paste0("dist_", type)
  
  # Subset SNAP retailers of the current Store Type
  stores_type <- sg[sg$Store.Type.Grouped == type, ]
  
  # Find the nearest store of the current type for each home
  nearest_store_indices <- st_nearest_feature(sfh_final, stores_type)
  
  # Calculate distances to the nearest store
  distances <- st_distance(sfh_final, 
                           stores_type[nearest_store_indices, ], 
                           by_element = TRUE)
  
  # Assign distances to the home data (convert to numeric, e.g., meters)
  sfh_final[[dist_col]] <- as.numeric(distances)
}

# Plot new distance features vs. log assessed total
plot(sfh_final$dist_Supermarket_store, sfh_final$Log_Assessed_Total)
plot(sfh_final$`dist_Convenience Store`, sfh_final$Log_Assessed_Total)
plot(sfh_final$`dist_Grocery Store`, sfh_final$Log_Assessed_Total)
plot(sfh_final$dist_Other, sfh_final$Log_Assessed_Total)

#'
#' Comment what we observe
#' 
#' For our second approach, we calculate the total number of stores present
#' within a 1 mile, 2 mile, and 10 mile radius for each property. We chose
#' these radii because they are used by the USDA to define food deserts.
#' 

# Compute distance matrix
dist_matrix <- st_distance(sfh_final, sg, by_element = FALSE)

# ~1 mi = 1609 m, ~2 mi = 3218 m, and ~10 mi = 16090 m
radii_labels <- c("1_mile", "2_miles", "10_miles")
radii_meters <- c(1609, 3218, 16090)

for (i in 1:3) {
  # Count the number of stores within the distance for each home
  counts <- apply(dist_matrix, 1, function(x) sum(x <= radii_meters[i]))
  
  # Add the counts to the data frame with a descriptive column name
  column_name <- paste0("total_", radii_labels[i])
  sfh_final[[column_name]] <- counts
}

# Plot new count features vs. log assessed total
plot(sfh_final$total_1_mile, sfh_final$Log_Assessed_Total)
plot(sfh_final$total_2_miles, sfh_final$Log_Assessed_Total)
plot(sfh_final$total_10_miles, sfh_final$Log_Assessed_Total)

#'
#' Comment what we observe
#'


#' 
#' # Modeling
#' ## Baseline Model
#' 
#' Make sure to explain what we're doing here, add commentary throughout
#' 

# Fit baseline model
m1 <- lm(Log_Assessed_Total ~ Sqrt_Effective_Area + Number_of_Bedroom + 
         Number_of_Baths_Total + Condition_Description + Zone_Category + 
         Effective_Age, data = sfh_final)

# Check model summary
summary(m1)

# Diagnostic plots
par(mfrow = c(2, 2))
par(mar = c(4, 4, 1.5, 1.5)) 
plot(m1, cex.lab = 0.8)

#' 
#' ## Proposed Model
#' 
#' Make sure to explain what we're doing here, add commentary throughout
#' 

# Fit proposed model with food access features
m2 <- lm(Log_Assessed_Total ~ Sqrt_Effective_Area + Number_of_Bedroom + 
         Number_of_Baths_Total + Condition_Description + Zone_Category + 
         Effective_Age + `dist_Convenience Store` + dist_Other + 
         `dist_Grocery Store` + dist_Supermarket_store + total_1_mile + 
         total_2_miles + total_10_miles, data = sfh_final)

# Check model summary
summary(m2)

# Diagnostic plots
par(mfrow = c(2, 2))
par(mar = c(4, 4, 1.5, 1.5)) 
plot(m2, cex.lab = 0.8)

# Model comparison
anova(m1, m2)

#' 
#' # Conclusion
#' 
#' Make sure to mention limitations, takeaways
#' Maybe a future suggestion would be to take spatial clustering of regions
#' into account
#' Multicollinearity - how could we address this since we're interested in inference
#' 
#' 