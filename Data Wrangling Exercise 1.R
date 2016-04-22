# Data Wrangling Exercise 1: Basic Data Manipulation
# Exercise
# Using R, clean the data set to make it easier to visualize and analyze.

# 0: Load the data set as a CSV file refine_original.csv in RStudio

mydata <- read.csv("refine_original.csv", header = TRUE, stringsAsFactors = FALSE)

str(mydata)
head(mydata)
tail(mydata)
names(mydata)

# 1: Clean up brand names

mydata$company
clean_company_name <- function(clean){
  clean <- tolower(clean)
  n <- nchar(clean)
  first <- substr(clean,1,2)
  last <- substr(clean,n-1,n)
  
  if(last == 'ps'){
    return("philips")
  }
  else if(first == 'ak'){
    return("akzo")
  }
  else if(first == 'va'){
    return("van houten")
  }  
  else{
    return("unilever")
  }
}

mydata$company <- sapply(mydata$company, FUN = clean_company_name)
mydata$company <- factor(mydata$company)
mydata$company

# 2: Seperate product code and number

product_codes <- strsplit(mydata$Product.code...number,split='-')
mydata$product_code <- sapply(product_codes,FUN = function(x) x[1])
mydata$product_number <- sapply(product_codes,FUN = function(x) x[2])
mydata$product_number <- as.integer(mydata$product_number)

# 3: Add product category

table <- c('p'='Smartphone','v'='TV','x'='Laptop','q'='Tablet')
mydata$product_category <- factor(table[mydata$product_code])
mydata$product_category

# 4: Add full address for geocoding

mydata$full_address = paste(mydata$address,mydata$city,mydata$country, sep = ",")
names(mydata)

# 5: Create dummy variables for company and product category

mydata$company_philips <- ifelse(mydata$company == "philips",1,0)
mydata$company_akzo <- ifelse(mydata$company == "akzo",1,0)
mydata$company_van_houten <- ifelse(mydata$company == "van houten",1,0)
mydata$company_unilever <- ifelse(mydata$company == "unilever",1,0)

mydata$product_smartphone <- ifelse(mydata$product_category == "Smartphone",1,0)
mydata$product_tv <- ifelse(mydata$product_category == "TV",1,0)
mydata$product_laptop <- ifelse(mydata$product_category == "Laptop",1,0)
mydata$product_tablet <- ifelse(mydata$product_category == "Tablet",1,0)

names(mydata)
mydata

# 6: Submit the project on Github

write.csv(x = mydata, file ="refine_clean.csv")