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

# For now, we drop the geometry for efficiency
dng <- st_drop_geometry(d)
saveRDS(dng, "dng.rds")
d <- readRDS("dng.rds")

#' ## Exploratory Data Analysis

#' ### Investigating Town-by-Town Data Quality
# Investigate the number of missing values by town
table(d$Assessed_Total <= 0, d$Town_Name, useNA = 'always')

# Investigate living area by town
table(d$Living_Area <= 0, d$Town_Name, useNA = 'always')

# Investigate buildings by town
table(d$Assessed_Building <= 0, d$Town_Name, useNA = 'always')



#' ### Initial Single Family Home Exploration
#' Here, we work with the larger dataset still
#' Observe below that state use descriptions are unreliable, but also that 
#' multiple state use codes can correspond to single family homes
d$State_Use_Description <- toupper(d$State_Use_Description)

# Filter for codes that contain the word Family
fam_df <- d[grepl("FAMILY", d$State_Use_Description) & 
              !grepl("[2-9]", d$State_Use_Description) & 
              !grepl("TWO", d$State_Use_Description) & 
              !grepl("THREE", d$State_Use_Description) & 
              !grepl("FOUR", d$State_Use_Description) & 
              !grepl("FIVE", d$State_Use_Description) & 
              !grepl("SIX", d$State_Use_Description) & 
              !grepl("MULTI", d$State_Use_Description), ]

# View State Use Description Table
table(fam_df$State_Use_Description)

# View State Use Table
table(fam_df$State_Use)

# Display Table of Mappings between State Use Description and State Use
table(fam_df$State_Use_Description, fam_df$State_Use)

# Observe that 1 Family does not appear does not use State Use Codes, look more
family_1 <- fam_df[fam_df$State_Use_Description == "1 FAMILY", ]
table(family_1$State_Use)

# Observe that this code is only for the town of Colebrook
table(family_1$Town_Name)


# Zooming in for now on 101 and 1010, we see there are different descriptions
check_df_101 <- d[d$State_Use == "101", ]
check_df_1010 <- d[d$State_Use == "1010", ]
table(check_df_101$State_Use_Description)
table(check_df_1010$State_Use_Description)

# 1011 looks to be single family homes with an accessory dwelling
check_df_1011 <- d[d$State_Use == "1011", ]
table(check_df_1011$State_Use_Description)

#' To briefly gauge town data quality, we view the distribution of single family
#' homes with these two codes of 101 and 1010 in each town, looking also at NAs.
d$sh <- ifelse(is.na(d$State_Use), NA, 
               ifelse(d$State_Use %in% c("101", "1010"), 
                      "Single", "Others"))
table(d$sh, d$Town_Name, useNA = 'always')

#' The distribution of NAs and how many single family homes are captured by the
#' two codes depends a lot on the town, although note that the percentage of
#' single family homes in different Connecticut towns also varies.
#' 
#' Going forward, let's zoom into a particular town/city: New Haven.
#' We focus on cleaning this data to identify other single family homes
#' that might not be captured by the data.

#' ### New Haven Single Family Home Analysis

# First, subset the original dataset to only New Haven
dnh <- d[d$Town_Name == "NEW HAVEN", ]

#' Some initial observations show the many codes and descriptions, we comment
#' this out to reduce clutter in the output.
# table(dnh$State_Use)
# table(dnh$State_Use_Description)
# table(dnh$State_Use, dnh$State_Use_Description, useNA = 'always')


#' We saw some descriptions that were listed as EXEMPT, we inspected them.
#' We observed these properties were non-profit or tax-exempt organization
#' buildings. Although these organizations could own single family homes, they
#' weren't found with this description
exempt <- dnh[grepl("Exempt", dnh$State_Use_Description, ignore.case = TRUE),]

#' We repeat the same analysis above looking at the descriptions with FAMILY
fam_nh_df <- dnh[grepl("FAMILY", dnh$State_Use_Description) & 
              !grepl("[2-9]", dnh$State_Use_Description) & 
              !grepl("TWO", dnh$State_Use_Description) & 
              !grepl("THREE", dnh$State_Use_Description) & 
              !grepl("FOUR", dnh$State_Use_Description) & 
              !grepl("FIVE", dnh$State_Use_Description) & 
              !grepl("SIX", dnh$State_Use_Description) & 
              !grepl("MULTI", dnh$State_Use_Description), ]

#' Observe that in New Haven, descriptions containing FAMILY that do not 
#' refer to multi-family homes are all captured by description SINGLE FAMILY
#' and all of these codes are 1010.  
table(fam_nh_df$State_Use, fam_nh_df$State_Use_Description, useNA = 'always')

#' In the whole dataset, we see that 1010 always corresponds to SINGLE FAMILY
dnh_1010 <- dnh[dnh$State_Use == "1010", ]
table(dnh_1010$State_Use_Description)

#' Each state use description has a 1-to-1 match to a code in New Haven
tapply(dnh$State_Use_Description, 
       dnh$State_Use, 
       function(x) length(unique(x)))

#' Get the corresponding State_Use_Description values for each State_Use
#' Observe that 101x codes correspond to single family homes, with the
#' last digit indicating some type of accessory/special feature

tapply(dnh$State_Use_Description, dnh$State_Use, unique)

# Create the initial filter for single family homes
sfh_coded <- dnh[which(startsWith(dnh$State_Use, "101")), ]
dim(sfh_coded)

#' TODO: Consider some other candidates for single family homes
#' The IND LD set perhaps

#' Now, let's consider potential NA values
# Check if any State_Use corresponds to an NA in State_Use_Description
na_desc <- tapply(dnh$State_Use_Description, 
                       dnh$State_Use, 
                       function(x) any(is.na(x)))
# Get the State_Use values that correspond to an NA in State_Use_Description
state_use_with_na_desc <- names(na_desc[na_desc == TRUE])
state_use_with_na_desc

# Check if any State_Use_Description corresponds to an NA in State_Use
na_state_use <- tapply(dnh$State_Use, 
                       dnh$State_Use_Description,
                       function(x) any(is.na(x)))
state_desc_with_na_code <- names(na_state_use[na_state_use == TRUE])

#' Observe a lot of descriptions do not have a corresponding code
state_desc_with_na_code

#' #### Focus on properties with no state use code
na_df <- dnh[is.na(dnh$State_Use), ]

table(na_df$State_Use_Description, useNA = 'always')

hsngauth <- dnh[(grepl("HSNG AUTH", dnh$State_Use_Description, ignore.case = TRUE) &
                  !dnh$State_Use_Description == "HSNG AUTH  MDL-00"),]

dim(hsngauth)

table(hsngauth$State_Use_Description)

# Inspect some addresses
# hsngauth[hsngauth$State_Use_Description == "HSNG AUTH  MDL-96", ]
hsng94 <- hsngauth[hsngauth$State_Use_Description == "HSNG AUTH  MDL-94", ]

#' Takeaway: we can remove houses with State_Use_Description in c("HSNG AUTH  MDL-96", "HSNG AUTH  MDL-00")
#' However, we should include c("HSNG AUTH  MDL-01")
#' Meanwhile, "HSNG AUTH  MDL-94" has a mixed bag of properties.
#' While 121 STUYVESANT AV seems to be a single family home online,
#' 145 Eastern ST and 408 Valley St look like complexes.

#' Note however, there's also HSG AUTH
#' Viewing these addresses, we find that they are 2 and 3 family homes
hsgauth <- dnh[grepl("HSG AUTH", dnh$State_Use_Description, ignore.case = TRUE),]
dim(hsgauth)


#' Now consider ones beginning with CITY 
city <- dnh[grepl("CITY", dnh$State_Use_Description, ignore.case = TRUE),]

table(city$State_Use_Description)

#' Inspecting these online, we find they are strange city-related
#' buildings that are not (or no longer) single family homes
# city[city$State_Use_Description == "CITY  MDL-96", ]

#' Note that these have Assessed Building of 0 and many are roads
#' online inspections also suggest they are not single family homes
# city[city$State_Use_Description == "CITY  MDL-00", ]

#' Let's adopt the heuristic of inspecting places where buildings have value
city_not_0 <- city[city$Assessed_Building != 0,]
table(city_not_0$State_Use_Description)

#' Among these codes, we find that CITY  MDL-01 are single family homes
dim(city[city$State_Use_Description == "CITY  MDL-01", ])

#' Interestingly, MDL-94 does not indicate single family homes for CITY
# city[city$State_Use_Description == "CITY  MDL-94", ]

# Through that, we discover that MDL-01 indicates single family homes
mdl01 <- dnh[grepl("MDL-01", dnh$State_Use_Description),]
dim(mdl01)


#' Let's look at MDL-94 again overall. All the ones spotchecked were not
#' single family homes.
#' Perhaps the single family homes in HSNG AUTH are mistakes
mdl94 <- dnh[grepl("MDL-94", dnh$State_Use_Description),]
table(mdl94$State_Use_Description)

#' candidate "single family home set"
sfh <- rbind(mdl01, hsng94, sfh_coded)
dim(sfh)

#' TODO: Consider if we should include condos and apartments.
# (Matt: definitely not apartments, but maybe condos)
# If we do, definitely need an additional category/indicator variable for them
apt <- dnh[dnh$State_Use_Description %in% c("APT 4-UNIT","APT5 - 12  MDL-94"), ]
dim(apt)

cond <- dnh[dnh$State_Use_Description %in% c("CONDOMINIUM"), ]
dim(cond)

#' According to the new haven equity profile
#' https://www.ctdatahaven.org/sites/ctdatahaven/files/new_haven_profile_v1.pdf
#' 23% of homes are single family out of 55682 housing units
.23*55682

# Consider how well this matches with our above data
sfh_rows <- nrow(sfh)
apt_rows <- nrow(apt)
cond_rows <- nrow(cond)

sfh_rows + cond_rows



# Not Affluent Areas vs Affluent Areas
# STEP 1: Focus on (1) New Haven vs (2) Greenwich







h <- d[d$State_Use == "101" | is.na(d$State_Use), ]
table(h$State_Use, useNA = 'always')
table(h$State_Use, h$Town_Name, useNA = 'always')

# We need better startegies
table(d$State_Use, d$Town_Name, useNA = 'always')



#' ### Assessed Total and Log-Transformation
#' Note that log-transforming the `Assessed_Total` variable produces a more 
#' normal-looking distribution
# [TODO: Code here]