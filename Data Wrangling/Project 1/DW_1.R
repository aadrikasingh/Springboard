# Load data and packages ===========================================================

library(dplyr)
library(tidyr)
store <- read.csv("refine_original.csv")

# Clean up brand names =============================================================

#transform the values to lowercase
store$company <- tolower(store$company)

#looking at the distinct values in the company column (should ideally be 4)
unique(store$company)

#standardize the values and remove misspellings (using regular expressions)
store$company <- gsub(pattern = "^a[[:alnum:][:space:]]*", replacement = "Akzo", x = store$company)

store$company <- gsub(pattern = "^[p,f][[:alnum:][:space:]]*", replacement = "Philips", x = store$company)

store$company <- gsub(pattern = "^v[[:alnum:][:space:]]*", replacement = "Van Houten", x = store$company)

store$company <- gsub(pattern = "^u[[:alnum:][:space:]]*", replacement = "Unilever", x = store$company)

# Separate product code and number =================================================

store <- separate(data = store, col = Product.code...number, into = c("product_code","product_number"), sep = "-")

# Add product categories ===========================================================

store$product_category <- ifelse(store$product_code == "p","Smartphone",
                                 ifelse(store$product_code == "v","TV",
                                        ifelse(store$product_code == "x","Laptop",
                                               ifelse(store$product_code == "q","Tablet",
                                                      "Unknown"))))

# Add full address for geocoding ===================================================

store <- unite(data = store, col = "full_address", ... = address:country, sep = ", ")

#If address, city, and country variables are to be kept
#store$full_address <- paste(store$address, store$city, store$country, sep = ", ")

# Create dummy variables for company ===============================================

store$company_philips <- as.numeric(store$company == "Philips")
store$company_akzo <- as.numeric(store$company == "Akzo")
store$company_van_houten <- as.numeric(store$company == "Van Houten")
store$company_unilever <- as.numeric(store$company == "Unilever")

# Create dummy variables for product category ======================================

store$product_smartphone <- as.numeric(store$product_category == "Smartphone")
store$product_tv <- as.numeric(store$product_category == "TV")
store$product_laptop <- as.numeric(store$product_category == "Laptop")
store$product_tablet <- as.numeric(store$product_category == "Tablet")

# Export CSV =======================================================================

write.csv(x = store, file = "refine_clean.csv")
