# Get housing data for 2017 to explore
# Import libraries for data wrangling

library(httr)
library(tidyr)
library(dplyr)
library(readr)
library(reshape2)
library(purrr)


# Use the txt file as it seems to work better without the " characters 
# that are in the csv.



# Generate the form for the url and download the files

make_url <- function(year) {
    paste("http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-",
          year,
          ".txt",
          sep = "")
}

urls <- map(seq(2012, 2016), make_url)

map(urls, function(x) {
    download.file(x, basename(x))
})

# Generate form for the filenames
make_filename <- function(year) {
    paste("pp-",
          year,
          ".txt",
          sep = "")
}

filenames <- map(seq(2012, 2016), make_filename)

# Read the files into memory, then bind the data frames by row and delete the 
# list of data frames to save memory
datalist <- map(filenames, 
                function(x) read_delim(x,
                                       delim = ",",
                                       col_names = FALSE,
                                       quoted_na = FALSE))

full_data <- bind_rows(datalist)

rm(datalist)

# Clean up the data: change certain columns to factors, set names for columns, 
# and reorder them.
full_data$X5 <- as.factor(full_data$X5)
full_data$X6 <- as.factor(full_data$X6)
full_data$X7 <- as.factor(full_data$X7)

full_data <- rename(full_data, tuid = X1, price = X2, date_of_transfer = X3, 
                    postcode = X4, property_type = X5, old_new = X6, 
                    duration = X7, paon = X8,saon = X9, 
                    street = X10, locality = X11, town = X12, 
                    district = X13, county = X14, ppd_type = X15, 
                    record_status = X16) %>% 
    separate(postcode, 
             into = c("outcode", "incode"), 
             sep = "\\ ",
             extra = "merge",
             fill = "right") %>% 
    select(price, date_of_transfer, 
           outcode, 
           incode,
           property_type, 
           everything())

# Group the data by outcode and property type, then summarise with a few key 
# stats
by_outcode_type <- full_data %>% 
    group_by(outcode, property_type) %>% 
    summarise(n_i = n(),
              avg_price = mean(price), 
              sd = sd(price), 
              threshold = quantile(price, 0.3))

# View top of the tibble and write out an intermediate summary file for Excel
by_outcode_type

write_excel_csv(by_outcode_type, "summarised_housing_data1_2017-03-28.csv")

# Check how many rows have a reasonable number of examples
nrow(by_outcode_type %>% 
         filter(n_i > 10))

# Use RStudoio Viewer to browse the tibble

View(by_outcode_type)
