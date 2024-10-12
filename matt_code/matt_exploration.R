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


#################################
########### PETER START #########
#################################

#' Here we explore the following
#"
#' #' 1. **Rooms**: Includes bedrooms, bathrooms, half-baths, and total rooms. 
#'    We anticipate a positive correlation between the number of rooms and 
#'    housing prices.
#' 
#' 2. **Size**: Property size, where larger properties are expected to have 
#'    higher prices. We exclude farm properties by focusing on single-family 
#'    households to avoid misclassification.
#' 
#' 3. **Zone**: Different zoning regulations (e.g., residential, commercial) 
#'    that affect property use, including permissions for commercial activities 
#'    like restaurants, which can influence housing prices.
#' 
#' 4. **Actual Year Built (AYB)**: Reflects property age. We anticipate that 
#'    older properties may be less expensive unless they have been renovated.
#' 
#' 5. **Condition**: Property condition categories (e.g., Excellent, Poor) 
#'    are expected to show variation in housing prices.
#'    
#'    
#'
#'
#' 
#' ### Rooms
#' 
#' 
#' (1) Number of Bedrooms
#' 
#' We find that the number of bedrooms is reasonable.
range(sfh$Number_of_Bedroom, na.rm = T)

#' We observe that there are a number of bedrooms that are
#' equal to 0 and has NA values. We inspect further to find out
#' what these properties are. 
table(sfh$Number_of_Bedroom, useNA = 'always')
zbed <- sfh[sfh$Number_of_Bedroom == 0 & !is.na(sfh$Number_of_Bedroom), ]
zbed

#' Since properties with 0 number of beds are small, we manually check
#' every property. After cross-checking with Vision Appraisal, it appears
#' that some NA values have been converted to 0. We fix the values for
#' these properties. In the process, we have further found out that there
#' are 2 duplicates in our data

# checked 115 CRANSTON ST: it is a 2 bed and 1 bath property [zillow]
# we observe that there are duplicates, we remove the wrong information
sfh <- sfh[!(sfh$Location == "115 CRANSTON ST" & sfh$Total_Rooms == 0), ]

# checked 532 CHAPEL ST: it is a 1 bed and 1 bath property [coldwell]
sfh$Number_of_Bedroom <- ifelse(sfh$Location == "532 CHAPEL ST", 1,
                                sfh$Number_of_Bedroom)

sfh$Number_of_Baths <- ifelse(sfh$Location == "532 CHAPEL ST", 1,
                              sfh$Number_of_Baths)

# checked 98 PARK ST: it is a 3 bed and 2 bath property [zillow]
sfh$Number_of_Bedroom <- ifelse(sfh$Location == "98 PARK ST", 3,
                                sfh$Number_of_Bedroom)

sfh$Number_of_Baths <- ifelse(sfh$Location == "98 PARK ST", 2,
                              sfh$Number_of_Baths)


# checked 348 ELM ST: it is a 6 bed and 2 bath property [zillow]
sfh$Number_of_Bedroom <- ifelse(sfh$Location == "348 ELM ST ", 6,
                                sfh$Number_of_Bedroom)

sfh$Number_of_Baths <- ifelse(sfh$Location == "348 ELM ST ", 2,
                              sfh$Number_of_Baths)

# checked 445 GEORGE ST: it is a 4bed and 2 bath property [zillow]
sfh$Number_of_Bedroom <- ifelse(sfh$Location == "445 GEORGE ST", 4,
                                sfh$Number_of_Bedroom)

sfh$Number_of_Baths <- ifelse(sfh$Location == "445 GEORGE ST", 2,
                              sfh$Number_of_Baths)

# checked 132 COVE ST: it is a studio: 0 bed and 1 bath property [trulia]
# checked 156 COVE ST: it is a studio: 0 bed and 2 bath property [trulia]

# Check Duplicates
ppp <- names(table(sfh$Location))
duplicates <- as.vector(table(sfh$Location) > 1)
ppp[duplicates]

# checked 1 RESERVOIR ST: 4 bed and 4 bath is the correct observation [zillow]
sfh <- sfh[!(sfh$Location == "1 RESERVOIR ST" & sfh$Number_of_Bedroom != 4), ]

#' Review the result:
#' It makes sense that most single family home properties have more than
#' 0 number of bedrooms. For the 2 properties, we have verified that they
#' were studios. 
sort(table(sfh$Number_of_Bedroom, useNA = 'always'))

#' Now, let's inspect properties with NA values in the number of bedrooms
nbed <- sfh[is.na(sfh$Number_of_Bedroom), ]

# Let's inspect the properties: Many of them are owned by City of New Haven!
table(nbed$Owner)

#' Since the number of data is manageable (< 30), we manually inspect the
#' observation using the internet and reassign the values correctly.
#' Since we have checked earlier that there are no duplicates in our data,
#' we are confident about this function! 

# Creating a customized function to assign the number of bedroom. 
assign_bed_bath <- function(loc, bed_input, bath_input) {
  
  # Create the location input directly as loc
  loc_input <- loc
  
  before_bed <- sum(is.na(sfh$Number_of_Bedroom))
  before_bath <- sum(is.na(sfh$Number_of_Baths))
  
  # Save the number of bedrooms appropriately!
  sfh$Number_of_Bedroom <- ifelse(sfh$Location == loc_input, 
                                  bed_input,
                                  sfh$Number_of_Bedroom)
  
  # Save the number of bathrooms appropriately!
  sfh$Number_of_Baths <- ifelse(sfh$Location == loc_input, 
                                bath_input,
                                sfh$Number_of_Baths)
  
  # To ensure that the function has been called
  after_bed <- sum(is.na(sfh$Number_of_Bedroom))
  after_bath <- sum(is.na(sfh$Number_of_Bath))
  
  print(paste0(
    "The number of bed & bath with NA value has been changed from", 
    "(", before_bed, ",", before_bath, ")", " to ",
    "(", after_bed, ",", after_bath, ")"))
  
  return(sfh)
  
}

#' We find some infomration online. For most properties, however,
#' the information is eithern ot available or they are apartments.
#' Since the location does not provie the full apartment number,
#' it appears that key information about the number of bedrooms and 
#' bathrooms are omitted from the data. 

# checked 121 STUYVESANT AV: 5bed, 2 bath [realtor]
sfh <- assign_bed_bath("121 STUYVESANT AV", 5, 2)

# checked 6 TOWNSEND AV: 2bed, 1 bath [redfin]
sfh <- assign_bed_bath("6 TOWNSEND AV", 2, 1)

# checked 23 CHAMBERLAIN ST #23: no information
# checked 145 EASTERN ST: no information, does not look like a house
# checked 1000 QUINNIPIAC AV: no information
# checked 1134 QUINNIPIAC AV: no information
# checked 408 VALLEY ST: no information
# checked 2 SO GENESEE ST: no information
# checked 436 VALLEY ST: no information
# checked 165 FRANKLIN ST : no information
# checked 29 JEFFERSON ST: no information
# checked 15 OLIVE ST: no information
# checked 49 UNION AV: no information
# checked 162 COLUMBUS AV: no information
# checked 84 PARK ST : no information

# checked 109 FRANK ST: 4 bed and 1.5 bath [zillow]
sfh <- assign_bed_bath("109 FRANK ST", 4, 2)

# checked 5 DAISY ST: no information, does not look like a house
# checked 578 GEORGE ST: no information
# checked 3 WAVERLY ST : no information
# checked 33 SYLVAN AV: no information 
# checked 819 SHERMAN AV: no information
# checked 220 COUNTY ST: no information

# checked 210 VALLEY ST: 2 bad and 1 bath [hotpads]
sfh <- assign_bed_bath("210 VALLEY ST", 2, 1)

# checked 295 WILMOT RD: no information, does not look like a house
# checked 34 BURTON ST: 0 bed (studio) and 1 bath [trulia] 
sfh <- assign_bed_bath("34 BURTON ST ", 0, 1)


# Now we are ready to drop observations that have NA values
# in the number of bedrooms. 
sfh <- sfh[!(is.na(sfh$Number_of_Bedroom)), ]


#' (2) Number of Bathrooms
#' 
#' There are 5 units with 0 number of bathrooms and 1 unit with NA. 
#' We check them manually!
table(sfh$Number_of_Baths, useNA = 'always')

# saving the dataframe with 0 or NA Bathrooms
nbath <- sfh[sfh$Number_of_Baths == 0 | is.na(sfh$Number_of_Baths), ]
nbath$Location

#' Again, there have been errors. These errors make sense given that
#' singe family houses usually have at least one bathroom.
#' We use `assign_bed_bath` function again to make correct transformations
#' 

# checked 1395 CHAPEL ST: dentistry, 2 beds has no info on bathroom
# we believe that there should be at least one bathroom hence assign 1 bath
sfh <- assign_bed_bath("1395 CHAPEL ST", 2, 1)

# checked 255 LEXINGTON AV : has 5 bed and 2 bath [realtor]
sfh <- assign_bed_bath("255 LEXINGTON AV ", 2, 1)

# checked 269 FRONT ST : has 3 bed and 0.5 bath [zillow]
sfh <- assign_bed_bath("269 FRONT ST ", 2, 1)

# checked 26 DOWNES ST: has 5 bed and 2 bath [zillow]
sfh <- assign_bed_bath(" 26 DOWNES ST", 2, 1)

# checked 132 COVE ST: has 0 bed (studio) and 1 bath [trulia] 
sfh <- assign_bed_bath("132 COVE ST", 2, 1)

# checked 156 COVE ST: has 0 bed (studio) and 2 bath [trulia]
sfh <- assign_bed_bath("156 COVE ST", 2, 1)


#' Final Check
range(sfh$Number_of_Bedroom)
range(sfh$Number_of_Baths)

# There are many properties that have zero half-baths
# and there are 40 NA values. Since half-baths are not
# typical in single households, we do not consider them 
table(sfh$Number_of_Half_Baths, useNA = 'always')

#' ###  Size
#' 
#' 
#' We consider (1) Effective Area and (2) Living Area
#' 
#' We observe that `Effective Area` is not equal to `Living Area`.
#' Otherwise, the variable looks great. There are no extreme values.
#' We may consider using `square root` to 
range(sfh$Effective_Area)
range(sfh$Living_Area)

hist(sfh$Effective_Area)
hist(sfh$Living_Area)

hist(sqrt(sfh$Effective_Area))
hist(sqrt(sfh$Living_Area)) 

hist(log(sfh$Effective_Area))
hist(log(sfh$Living_Area)) 

#' ### Condition

# There are 2 U values 328 F values.
# We inspect whether F means Fair, and what U looks like
table(sfh$Condition_Description, useNA = 'always')

#' Both properties with U are pretty old. 
#' It appears that U stands for Unknown or Unspecified.
#' We searched the web to find their conditions, and changed them
#' accordingly. 

sfh[sfh$Condition_Description == "U", ]

# checked 269 FRONT ST: Fair [redfin]
sfh$Condition_Description <- ifelse(sfh$Location == "269 FRONT ST ",
                                    "F",
                                    sfh$Condition_Description)

# checked 6 EVERGREEN CT: Average [redfin]
sfh$Condition_Description <- ifelse(sfh$Location == "6 EVERGREEN CT ",
                                    "Average",
                                    sfh$Condition_Description)

# Great
table(sfh$Condition_Description, useNA = 'always')

# Now check whether F means Fair
# checked 351 CONCORD ST - average [redfin]
# checked 100 FARREN AV - average [redfin, vision]
# checked 113 CEDAR ST - average [redfin, vision]
# checked 922 WINCHESTER AV - average[redfin]
head(sfh[sfh$Condition_Description == "F", ])

#' 
#' Due to many overlaps, we change F to Average
sfh$Condition_Description <- ifelse(sfh$Condition_Description == "F",
                                    "Average",
                                    sfh$Condition_Description)

sfh$Condition_Description <- factor(sfh$Condition_Description,
                                    levels = c("Very Poor", "Poor", "Average",
                                               "Good", "Excellent"
                                    ))

#' ### Zone
#' 
#' We have categorized different zones. Please see Appendix for details.
#' Please note that the zoning may not be accurate. However, we tried to
#' make reasonable categories in accordance with the reference materials
#' to ensure that our future model does not become too complex

# Categories (Simplified to 8):
residential <- c("RS1", "RS2", "RS1/RS2", "RM1", "RM2", "RM2/RS2", "RH1", "RH2")
mixed_use <- c("RO", "BA/RM1", "BA/RM2", "BA/RS2")
commercial <- c("BA", "BA1", "BB", "BC", "BD", "BD1")
industrial <- c("IL", "IH", "IH/RM2")
planned_development <- c("PDD 119", "PDD 26", "PDD 39", "PDD 49", "PDD 52", 
                         "PDU 102", "PDU 106", "PDU 108", "PDU 16", "PDU 72", 
                         "PDU 75", "PDU 95")
special_use <- c("CEM", "PARK")

# Assigning categories using ifelse
sfh$Zone_Category <- ifelse(sfh$Zone %in% residential, 
                            "Residential", sfh$Zone_Category)
sfh$Zone_Category <- ifelse(sfh$Zone %in% mixed_use, 
                            "Mixed-Use", sfh$Zone_Category)
sfh$Zone_Category <- ifelse(sfh$Zone %in% commercial, 
                            "Commercial", sfh$Zone_Category)
sfh$Zone_Category <- ifelse(sfh$Zone %in% industrial, 
                            "Industrial", sfh$Zone_Category)
sfh$Zone_Category <- ifelse(sfh$Zone %in% planned_development, 
                            "Planned Development", sfh$Zone_Category)
sfh$Zone_Category <- ifelse(sfh$Zone %in% special_use, 
                            "Special Use", sfh$Zone_Category)

# Checking the result
table(sfh$Zone_Category, useNA = 'always')


#' ### Actual Year Builty vs Effective Year Built
#'
range(sfh$AYB)
range(sfh$EYB)


#' ### Assessed Total
#' 
hist(sfh$Assessed_Total)
range(sfh$Assessed_Total)

#' After inspecting the data, it appears that there are many data entry errors
#' and includes golf clubs and other big area that we are less interseted in.
#' Therefore we remove them from our observaitons.

# checked 223 EAST GRAND AV: looks like a data errror
# checked 35 EASTERN ST: it is a golf club
# checked 1435 QUINNIPIAC AV: no information, 
# but it appears that refers to an entire set of apartments or properties
# checked 382 FERRY ST: this looks like data entry error
head(sfh[sfh$Assessed_Total > 1000000, ])


# We delete anything that costs more than 1M
sfh <- sfh[sfh$Assessed_Total < 1000000, ]

#' We look at the distribution of Assessed Total again
#' Looks great
hist(sfh$Assessed_Total)
range(sfh$Assessed_Total)

#' We further create a log of Assessed Total to ensure that it has a bell-shaped
#' distribution. The distribution looks amazing!
sfh$at_log <- log(sfh$Assessed_Total)
hist(sfh$at_log)


#' ## Exploratory Data Analysis
#' 

#' ###  Scatter plot: Bedrooms vs. Log-Assessed Total
plot(sfh$Number_of_Bedroom, sfh$at_log, 
     main = "Bedrooms vs Log-Assessed Total", 
     xlab = "Number of Bedrooms", 
     ylab = "Log-Assessed Total")

#' ###  Scatter plot: Bathrooms vs. Log-Assessed Total
plot(sfh$Number_of_Baths, sfh$at_log, 
     main = "Baths vs Log-Assessed Total", 
     xlab = "Number of Bathrooms", 
     ylab = "Log-Assessed Total")

#' ### Scatter plot: AYB (Actual Year Built) vs. Log-Assessed Total
plot(sfh$AYB, sfh$at_log, 
     main = "AYB vs Log-Assessed Total", 
     xlab = "Actual Year Built", 
     ylab = "Log-Assessed Total")

#' ### Scatter plot: EYB (Effective Year Built) vs. Log-Assessed Total
plot(sfh$EYB, sfh$at_log, 
     main = "EYB vs Log-Assessed Total", 
     xlab = "Actual Year Built", 
     ylab = "Log-Assessed Total")

#' ###  Boxplot: Log-Assessed Total by Condition Description
boxplot(sfh$at_log ~ sfh$Condition_Description, 
        main = "Boxplot of Log-Assessed Total by Condition", 
        xlab = "Condition", 
        ylab = "Log-Assessed Total")

#' ###  Boxplot: Log-Assessed Total by Zone
boxplot(sfh$at_log ~ sfh$Zone_Category, 
        main = "Boxplot of Log-Assessed Total by Zone", 
        xlab = "Zone", 
        ylab = "Log-Assessed Total")

#################################
########### PETER END ###########
#################################

#' ### Grouping Property Zones in New Haven, CT
#' 
#' The property zones in New Haven, CT, can be categorized into broader categories based on their designations and usage. Here's a suggested grouping:
#' 
#' ### Grouping Property Zones in New Haven, CT
#' 
#' The property zones in New Haven, CT, can be categorized into broader categories 
#' based on their designations and usage. Here's a suggested grouping:
#' 
#' 1. **Residential Zones**:
#'    - `RS1`, `RS2`, `RS1/RS2`, `RM1`, `RM2`, `RM2/RS2`, `RH1`, `RH2`
#'      # Includes single-family, multi-family, and high-density residential areas
#' 
#' 2. **Mixed-Use Zones**:
#'    - `RO`, `BA/RM1`, `BA/RM2`, `BA/RS2`
#'      # Zones allowing both residential and commercial use
#' 
#' 3. **Commercial Zones**:
#'    - `BA`, `BA1`, `BB`, `BC`, `BD`, `BD1`
#'      # Business, retail, and commercial areas
#' 
#' 4. **Industrial Zones**:
#'    - `IL`, `IH`, `IH/RM2`
#'      # Zones designated for light and heavy industrial activities
#' 
#' 5. **Planned Development Zones**:
#'    - `PDD 119`, `PDD 26`, `PDD 39`, `PDD 49`, `PDD 52`, `PDU 102`, 
#'      `PDU 106`, `PDU 108`, `PDU 16`, `PDU 72`, `PDU 75`, `PDU 95`
#'      # Planned developments and units for specific projects
#' 
#' 6. **Special Use Zones**:
#'    - `CEM`, `PARK`
#'      # Parks, cemeteries, and other special-purpose areas

#' 
#' **Notes**: 
#' - Categories such as `Residential`, `Commercial`, `Industrial`, and `Planned Development` follow typical urban zoning categorizations.
#' - Some zones (e.g., `NA` values) are unspecified and may need further investigation to determine their appropriate category.
#' 
#' **Resources Used**: 
#' - Standard zoning classifications and New Haven city zoning documentation (https://www.newhavenct.gov/government/departments-divisions/planning-and-zoning).



#' ### Assessed Total and Log-Transformation
#' Note that log-transforming the `Assessed_Total` variable produces a more 
#' normal-looking distribution
# [TODO: Code here]