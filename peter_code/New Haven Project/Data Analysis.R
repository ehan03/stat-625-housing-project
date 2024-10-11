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
#' ## Project Description
#' 
#' This project examines whether access to food, measured by proximity to 
#' grocery stores, impacts housing prices. We focus specifically on single 
#' family households in Connecticut to explore the relationship between 
#' housing prices and access to food resources.
#' 
#' ## Data Description
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
#' ## Model and Key Variable Description
#' 
#' We employ a linear model to investigate the relationship between housing 
#' prices and various property attributes:
#' 
#' 1. **Rooms**: Includes bedrooms, bathrooms, half-baths, and total rooms. 
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
#' \newpage
#' 

## Import Libraries and Data
library(sf)
library(lme4)

# Read Data
dr <- readRDS("cleaned.rds")

#' ## Exploratory Data Analysis
#' 
#' ### Scatter plot: Total Rooms vs. Log-Assessed Total
plot(dr$total_rooms, dr$at_log, 
     main = "Total Rooms vs Log-Assessed Total", 
     xlab = "Total Rooms", 
     ylab = "Log-Assessed Total")

#' ###  Scatter plot: Bedrooms vs. Log-Assessed Total
plot(dr$number_of_bedroom, dr$at_log, 
     main = "Bedrooms vs Log-Assessed Total", 
     xlab = "Number of Bedrooms", 
     ylab = "Log-Assessed Total")

#' ###  Scatter plot: Bathrooms vs. Log-Assessed Total
plot(dr$number_of_baths, dr$at_log, 
     main = "Baths vs Log-Assessed Total", 
     xlab = "Number of Bathrooms", 
     ylab = "Log-Assessed Total")

#' ### Scatter plot: AYB (Actual Year Built) vs. Log-Assessed Total
plot(dr$ayb, dr$at_log, 
     main = "AYB vs Log-Assessed Total", 
     xlab = "Actual Year Built", 
     ylab = "Log-Assessed Total")

#' ###  Boxplot: Log-Assessed Total by Condition Description
boxplot(dr$at_log ~ dr$condition_description, 
        main = "Boxplot of Log-Assessed Total by Condition", 
        xlab = "Condition", 
        ylab = "Log-Assessed Total")

#' ###  Boxplot: Log-Assessed Total by Zone
boxplot(dr$at_log ~ dr$zone, 
        main = "Boxplot of Log-Assessed Total by Zone", 
        xlab = "Zone", 
        ylab = "Log-Assessed Total",
        horizontal = TRUE
)

#' \newpage
#' 
#' ## Modeling
#' 
#' ### Linear models to explore relationships
#' 
#' The base model explores the effect of rooms, condition, and zone on the 
#' log-transformed assessed total (`at_log`). The model includes bedrooms, 
#' bathrooms, and living area as predictors.

# Fit the linear model
mod_b <- lm(at_log ~ total_rooms + number_of_bedroom + number_of_baths + 
              condition_description + living_area + zone, data = dr)

# Save and display the summary of the model
mod_b_summary <- summary(mod_b)
mod_b_summary  # Display the summary of the model

# Display residual diagnostics
par(mfrow = c(2, 2))
plot(mod_b, main = "Residual Diagnostics for Base Model")

# Reset the plotting layout to its default
par(mfrow = c(1, 1))

#' ### Mixed Effects Model
#' 
#' We treat zone as a random effect to model the variability across zones 
#' without estimating separate coefficients for each. This approach ensures 
#' generalizability and avoids overfitting. Treating it as a fixed effect 
#' would result in over 100 coefficients, leading to an overly complex model.

# Fit the mixed effects model with zone as a random effect
mod_b_mixed <- lmer(at_log ~ total_rooms + number_of_bedroom + 
                      number_of_baths + condition_description + 
                      (1 | zone) + living_area, data = dr)

# Display the summary of the mixed effects model
summary(mod_b_mixed)


#' \newpage
#' # Conclusion
#' 
#' 
#' # Sessioninfo
#' 
sessionInfo()
