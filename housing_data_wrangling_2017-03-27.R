# Get housing data for 2017 to explore
library(httr)
library(tidyr)
library(dplyr)
library(readr)
library(reshape2)
library(purrr)
# Use the txt file as it seems to work better without the " characters 
# in the csv
url <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2017.txt"
url <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2016.txt"
download.file(url, "pp-2017.txt", method = "wget")

# Read in file from .txt
df <- read_delim("pp-2017.txt", 
                 delim = ",",
                 col_names = FALSE,
                 quoted_na = FALSE)

# Tidy up column classes and names
df$X5 <- as.factor(df$X5)
df$X6 <- as.factor(df$X6)
df$X7 <- as.factor(df$X7)

df <- rename(df, tuid = X1, price = X2, date_of_transfer = X3, postcode = X4,
             property_type = X5, old_new = X6, duration = X7, paon = X8,
             saon = X9, street = X10, locality = X11, town = X12, district = X13,
             county = X14, ppd_type = X15, record_status = X16) %>% 
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

head(df)

by_outcode_type <- df %>% 
    group_by(outcode, property_type) %>% 
    summarise(n = n(),
              avg_price = mean(price), 
              sd = sd(price), 
              threshold = quantile(price, 0.3))

head(by_outcode_type, n = 200)

years <- seq(2012, 2016)
make_url <- function(year) {
    paste("http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-",
          year,
          ".txt",
          sep = "")
}
urls <- map(years, make_url)

map(urls, function(x) {
    download.file(x, basename(x))
})

make_filename <- function(year) {
    paste("pp-",
          year,
          ".txt",
          sep = "")
}

filenames <- map(years, make_filename)

datalist <- lapply(filenames, function(x) read_delim(x, 
                                                     delim = ",",
                                                     col_names = FALSE,
                                                     quoted_na = FALSE))

full_data <- do.call("rbind", datalist)

df_2012 <- read_delim("pp-2012.txt", 
                      delim = ",",
                      col_names = FALSE,
                      quoted_na = FALSE)

df_2013 <- read_delim("pp-2013.txt", 
                      delim = ",",
                      col_names = FALSE,
                      quoted_na = FALSE)

