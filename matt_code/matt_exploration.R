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
#' * (1) property data for the state of Connecticut and 
#' * (2) Connecticut Supplemental Nutrition Assistance Program (SNAP) Retailers
#' 
#' The latter dataset was obtained from the CT Open Data Portal and includes
#' latitudes and longitudes for eligible food stores, as well as categories
#' for what the stores are.
#' 
#' Meanwhile, the property data details the value and other features of 
#' properties in Connecticut. It includes POLYGON data describing the location.
#' 
#' \newpage
#' # Data Pre-Processing
#' 
#' ## Import Libraries and Data
library(sf)

# Load the Connecticut Property Shapefile (drop geometry for now for efficiency)
d <- st_read("../data/CT-parcel-data/4c5501b8-b68e-4888-bf6a-d92670d69c3b.gdb/")

dng <- st_drop_geometry(d)

saveRDS(dng, "dng.rds")

#' ## Exploratory Data Analysis
#' 
#' ### Assessed Total and Log-Transformation
#' Note that log-transforming the `Assessed_Total` variable produces a more 
#' normal-looking distribution
# [TODO: Code here]


#' ### Identifying a subset of single family homes
#' Observe below that state use descriptions are unreliable, but also that 
#' multiple state use codes can correspond to single family homes

# table(d$Assessed_Total <= 0, d$Town_Name, useNA = 'always')

fam_df <- d[grepl("Family", d$State_Use_Description, ignore.case = TRUE) & 
              !grepl("[2-9]", d$State_Use_Description) & 
              !grepl("TWO", toupper(d$State_Use_Description)) & 
              !grepl("THREE", toupper(d$State_Use_Description)) & 
              !grepl("FOUR", toupper(d$State_Use_Description)) & 
              !grepl("FIVE", toupper(d$State_Use_Description)) & 
              !grepl("SIX", toupper(d$State_Use_Description)), ]


table(fam_df$State_Use)
table(fam_df$State_Use_Description)
table(fam_df$State_Use_Description, fam_df$State_Use)

# Observe that 101 and 1010
check_df_101 <- d[d$State_Use == "101", ]
check_df_1010 <- d[d$State_Use == "1010", ]
table(check_df_101$State_Use_Description)
table(check_df_1010$State_Use_Description)

# 1011 is not a suitable category for our purpose
# it's a different type
check_df_1011 <- d[d$State_Use == "1011", ]
table(check_df_1011$State_Use_Description)

# View the distribution of single family based off these two codes
# Consider if other codes have Single Family homes
d$sh <- ifelse(is.na(d$State_Use), "NA", 
               ifelse(d$State_Use %in% c("101", "1010"), 
                      "Single", "Others"))
table(d$sh, d$Town_Name, useNA = 'always')

# Let's look at the descriptions of all properties not captured by 101, 1010, 1011
unique(c[!(c$State_Use %in% c("101", "1010", "1011")), ]$State_Use_Description)
# Interesting descriptions where we want to see the code: 
# Sin Fam WF, Single Fam, 1 Family Planned Comm, Single Fam M00, SFam BPW

#' Let's focus on the specific cities (New Haven, Greenwich) and do the above
#' analysis again. We might want to look more at what other features can help
#' us identify single family homes.

#table(c[!(c$State_Use %in% c("101", "1010", "1011")), ]$State_Use_Description)


# Single family homes typically have a limited number of bedrooms
# We filter for 4 rooms and below to look through what other codes could be
# candidates for single family homes
c <- d[d$Number_Of_Bedroom < 5, ]
cc <- c[!(c$State_Use %in% c("101", "1010")), ]
# c$State_Use %in% c("100")
table(c[(c$State_Use %in% c("100")), ]$State_Use_Description)


# Not Affluent Areas vs Affluent Areas
# STEP 1: Focus on (1) New Haven vs (2) Greenwich







h <- d[d$State_Use == "101" | is.na(d$State_Use), ]
table(h$State_Use, useNA = 'always')
table(h$State_Use, h$Town_Name, useNA = 'always')

# We need better startegies
table(d$State_Use, d$Town_Name, useNA = 'always')


