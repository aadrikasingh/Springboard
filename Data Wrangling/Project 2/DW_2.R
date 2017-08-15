# Load packages and data ================================================================

library(dplyr)
library(tidyr)

ship <- read.csv("titanic_original.csv")

str(ship)
summary(ship)

# Port of embarkation ===================================================================

#Replace missing values with S
str(ship$embarked)
ship$embarked[ship$embarked == ''] <- "S"

# Age ===================================================================================

ship$age[is.na(ship$age)] <- mean(ship$age, na.rm = TRUE)

# Lifeboat ==============================================================================

levels(ship$boat)
levels(ship$boat)[1] <- "None"

# Cabin =================================================================================

ship$has_cabin_number <- as.numeric(ship$cabin != "")
table(ship$has_cabin_number)

# Create CSV file =======================================================================

write.csv(x = ship, file = "titanic_clean.csv")
