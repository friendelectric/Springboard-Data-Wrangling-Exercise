# loading libraries
library(stringr)
library(dplyr)
library(tibble)

## 0: loading data into R: setting working directory, loading original .csv file

#setwd("3 Data Wrangling")

refine_original <- read.csv("refine_original.csv")

# converting to table format for use w/ dplyr
ndtech <- tbl_df( refine_original )

## 1: cleaning up brand names: removing misspellings, transforming to lower case

# lower case company column, remove prior version via mutate
ndtech <- ndtech %>% 
  mutate(company = tolower(company))

# substituting misspellings
ndtech$company <- gsub( regex("^ph.*ps$"), "philips",  ndtech$company )
ndtech$company <- gsub( regex("^f.*ps$"),  "philips",  ndtech$company )
ndtech$company <- gsub( regex("^ak.*"),    "akzo",     ndtech$company )
ndtech$company <- gsub( regex("^un.*er$"), "unilever", ndtech$company )

## 2: separating product code and number

# renaming for convenience
ndtech <- rename( ndtech, product = Product.code...number)

# splitting the column into two, removing the old "product" column
ndtech <- ndtech %>% 
  mutate( product = str_split(ndtech$product, "-") ) %>%
  rowwise() %>% 
  mutate( product_code   = unlist(product)[1], 
          product_number = unlist(product)[2]) %>% 
  select(-product)

## 3: adding product categories

# function for reading product codes: input one-letter code (p,v,x,q), return product category name out of 4 product categories
read_product_code <- function(x) {
  product_code     <- c("p",          "v",  "x",      "q")
  product_category <- c("Smartphone", "TV", "Laptop", "Tablet")
  i <- 0
  for (i in 1:4) {
    if ( x == product_code[i] ) { break } else { i <- i+1 }
  }
  return(product_category[i])
}

# applying the function while mutating 
ndtech <- ndtech %>% 
  mutate( product_category = read_product_code(product_code) )

## 4: adding full address for geocoding

ndtech <- ndtech %>% 
  add_column( full_address = str_c( ndtech$address, ndtech$city, ndtech$country, sep = ", " ) ) 

## 5: creating dummy variables for company and product category

# function for category verification
# input: table column, comparison vector, string refering to category name (e.g., "philips", "Laptop")
verify_category <- function(x, y, z) {
  i <- 0
  for (i in 1:length(y)) {
    if ( y[i] == z ) { break } 
    i <- i+1
  }
  ifelse ( grepl(y[i],x), TRUE, FALSE )
}

# assigning vectors for all possible values of the columns
product_category <- unique(ndtech$product_category)
company_category <- unique(ndtech$company)

# creating new columns w/ logicals for each company & category
ndtech <- ndtech %>% add_column( company_akzo       = verify_category(ndtech$company,          company_category, "akzo"      ) ) %>% 
                     add_column( company_philips    = verify_category(ndtech$company,          company_category, "philips"   ) ) %>% 
                     add_column( company_unilever   = verify_category(ndtech$company,          company_category, "unilever"  ) ) %>%
                     add_column( company_van_houten = verify_category(ndtech$company,          company_category, "van houten") ) %>%

                     add_column( product_smartphone = verify_category(ndtech$product_category, product_category, "Smartphone") ) %>%
                     add_column( product_tv         = verify_category(ndtech$product_category, product_category, "TV"        ) ) %>% 
                     add_column( product_laptop     = verify_category(ndtech$product_category, product_category, "Laptop"    ) ) %>%
                     add_column( product_tablet     = verify_category(ndtech$product_category, product_category, "Tablet"    ) )

# exporting wrangled data
write.csv(ndtech, "refine_clean.csv")