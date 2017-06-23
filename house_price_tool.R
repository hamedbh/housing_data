# Import libraries for data wrangling

require(tidyr)
require(dplyr)
require(readr)
require(reshape2)
require(purrr)
require(lubridate)
require(stringr)
require(magrittr)
require(broom)


# Function to download (if necessary) price paid data and prepare for analysis
create_full_data <- function() {
    
    make_url <- function(year) {
        paste0("http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-",
               year,
               ".txt")
    }
    
    # Set years parameter to allow for easily changing the scope of the data we get
    years <- seq(2015, 2016)
    
    # Create the urls and download the data
    urls <- map_chr(years, make_url)
    setwd("/home/rstudio/housing_data/")
    if(!dir.exists("./data.")) {
        dir.create("./data")
    }
    setwd("./data")
    walk(urls, function(x) {
        if(!file.exists(basename(x))) {
            download.file(x, basename(x), method = "wget")
        }
    })
    
    # Generate form for the filenames
    make_filename <- function(year) {
        paste("pp-",
              year,
              ".txt",
              sep = "")
    }
    
    filenames <- basename(urls)
    
    # Read the files into memory, then bind the data frames by row and delete the 
    # list of data frames to save memory
    datalist <- map(filenames, 
                    function(x) read_delim(x,
                                           delim = ",",
                                           col_names = FALSE,
                                           quoted_na = FALSE,
                                           col_types = "cicccccccccccccc"))
    
    full_data <- bind_rows(datalist)
    
    rm(datalist)
    
    # Clean up the data: change certain columns to factors, set names for columns, 
    # and reorder them.
    full_data$X5 <- as.factor(full_data$X5)
    full_data$X6 <- as.factor(full_data$X6)
    full_data$X7 <- as.factor(full_data$X7)
    
    full_data <- rename(full_data, tuid = X1, price = X2, date_of_transfer = X3, 
                        postcode = X4, property_type = X5, old_new = X6, 
                        duration = X7, paon = X8, saon = X9, 
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
    
    # Clean and standardise the dates, add column for year
    full_data[[2]] <- full_data[[2]] %>% 
        map(function(x) substr(x, 1, 10)) %>% 
        flatten_chr() %>% 
        as_date()
    
    full_data <- full_data %>% 
        mutate(year = year(date_of_transfer)) %>% 
        select(price, 
               date_of_transfer, 
               year, 
               outcode, 
               property_type, 
               incode, 
               everything())
    
    # Write out full_data to rds to save reprocessing
    write_rds(full_data, '~/housing_data/data/full_data.rds')
}

# If full_data is not already in memory the ifelse will first try to read in 
# rds file with the full_data tibble, otherwise will run the create_full_data 
# function.
if(!exists('full_data')) {
    ifelse(file.exists('~/housing_data/data/full_data.rds'),
           full_data <- read_rds('~/housing_data/data/full_data.rds'),
           create_full_data)
}

# Group the data by outcode and property type, then summarise with a few key 
# stats
if(!exists('by_outcde_typ')) {
    ifelse(file.exists('~/housing_data/data/by_outcde_typ.rds'),
           by_outcde_typ <- read_rds('~/housing_data/data/by_outcde_typ.rds'),
           full_data %>% 
               group_by(outcode, property_type) %>% 
               summarise(n = n(),
                         avg_price = mean(price), 
                         sd = sd(price),
                         q05 = quantile(price, .05),
                         q10 = quantile(price, .10),
                         q15 = quantile(price, .15), 
                         q20 = quantile(price, .20),
                         q25 = quantile(price, .25),
                         q50 = quantile(price, .50),
                         q75 = quantile(price, .75)) -> by_outcde_typ)
}

# Write out the tibble to rds
if(!file.exists('~/housing_data/data/by_outcde_typ.rds')) {
    write_rds(by_outcde_typ, '~/housing_data/data/by_outcde_typ.rds')
}

# View top of the tibble and write out an intermediate summary file for Excel
by_outcde_typ

write_excel_csv(by_outcde_typ, 
                "~/housing_data/data/by_outcde_typ.csv")

# Check how many groups have at least 10 data points
paste0(round((100 * nrow(by_outcde_typ %>% 
                     filter(n > 10)) / nrow(by_outcde_typ)),
            2),
       "%")

# Try grouping by year as well as type and outcode to examine changes in price 
# over time.
if(!exists('by_yr_outcde_typ')) {
    ifelse(file.exists('~/housing_data/data/by_yr_outcde_typ.rds'),
           by_yr_outcde_typ <- read_rds('~/housing_data/data/by_yr_outcde_typ.rds'),
           full_data %>% 
               group_by(year, 
                        outcode, 
                        property_type) %>% 
               summarise(n = n(),
                         avg_price = mean(price), 
                         sd = sd(price),
                         q05 = quantile(price, .05),
                         q10 = quantile(price, .10),
                         q15 = quantile(price, .15), 
                         q20 = quantile(price, .20),
                         q25 = quantile(price, .25),
                         q50 = quantile(price, .50),
                         q75 = quantile(price, .75)) -> by_yr_outcde_typ)
}
# Write out the tibble to rds
if(!file.exists('~/housing_data/data/by_yr_outcde_typ.rds')) {
    write_rds(by_yr_outcde_typ, '~/housing_data/data/by_yr_outcde_typ.rds')
}

# View top of the tibble and write out an intermediate summary file for Excel
by_yr_outcde_typ

write_excel_csv(by_yr_outcde_typ, 
                "~/housing_data/data/by_yr_outcde_typ.csv")

# Check how many rows have a reasonable number of examples
paste0(round((100 * nrow(by_yr_outcde_typ %>% 
                             filter(n > 5)) / nrow(by_yr_outcde_typ)),
             2),
       "%")

paste0(round((100 * nrow(by_yr_outcde_typ %>% 
                             filter(n > 10)) / nrow(by_yr_outcde_typ)),
             2),
       "%")

# Combine the Ordnance Survey postcode list csv files to get full list of all 
# UK outcodes.
fils <- list.files('~/housing_data/data/OS_data', 
                   pattern = '.csv$', 
                   full.names = TRUE)

pcdes <- map_df(fils, function(x) {
    
    read_csv(x, 
             col_names = FALSE,
             col_types = 'ciiicccccc')
    
}) %>% 
    select(pcde = X1) %>% 
    mutate(pcde = str_replace_all(pcde, " ", ""),
           outcde = str_sub(pcde, end = -4), 
           len_test = nchar(outcde), 
           pcde_test = str_detect(pcde, 
                                  "[A-z]{1,2}\\d{1,2}[A-z]?\\d[A-z]{2}"),
           outcde_test = str_detect(outcde,
                                    "[A-z]{1,2}\\d{1,2}[A-z]?"))

# Test whether all pcdes have required format
min(pcdes$pcde_test) == 1

# Test whether all outcdes have required format
min(pcdes$outcde_test) == 1

# Drop cols and duplicates from all_pcdes to save memory
pcdes %>% 
    select(outcde) %>% 
    distinct(.) %>% 
    add_row(outcde = '', .before = 1) -> outcdes

rm(pcdes)

# Prepare all_outcdes for binding with year and type data
bind_rows(outcdes, outcdes) %>% 
    bind_cols(., tibble(year = rep(2015:2016, 
                                   each = 2881))) %>% 
    inner_join(., 
               tibble(property_type = as.factor(rep(c('D', 'F', 'O', 'S', 'T'),
                                                    times = 2)),
                      year = rep(2015:2016, each = 5))) -> outcdes_yrs_grps

# Testing all_outcdes against price paid data, to see which have no/low data.
left_join(outcdes_yrs_grps,
          by_yr_outcde_typ,
          by = c('outcde' = 'outcode',
                 'year' = 'year',
                 'property_type' = 'property_type'
                 )) -> grp_ppdata

sapply(grp_ppdata, function(x) {max(is.na(x))})
sapply(grp_ppdata, function(x) {sum(is.na(x))})

grp_ppdata %>% 
    filter(is.na(n) | n < 10) -> low_no_data

# What proportion of transactions occur in areas with low/no data?
sum(low_no_data$n, na.rm = T) / sum(grp_ppdata$n, na.rm = T)

# Use first part of outcode to generate larger n groups that can be used to 
# examine changes in price over time

full_data <- full_data %>% 
    mutate(area_code = str_extract(outcode, "[A-Z]{1,2}")) %>% 
    select(price, 
           date_of_transfer, 
           year, 
           area_code, 
           outcode, 
           property_type, 
           incode, 
           everything())

by_area_yr_typ <- full_data %>%
    group_by(area_code, 
             property_type,
             year) %>% 
    summarise(n = n(),
              avg_price = mean(price), 
              sd = sd(price), 
              q05 = quantile(price, .05),
              q10 = quantile(price, .10),
              q15 = quantile(price, .15), 
              q20 = quantile(price, .20),
              q25 = quantile(price, .25),
              q50 = quantile(price, .50),
              q75 = quantile(price, .75))

# Create an areas_yrs_grp table to check for missing data
outcdes_yrs_grps %>% 
    mutate(area_cde = str_extract(outcde, "[A-z]{1,2}")) %>% 
    select(area_cde, year, property_type) %>% 
    distinct(.) -> areas_yrs_grps

# Set NAs to empty string
areas_yrs_grps[is.na(areas_yrs_grps$area_cde), 1] <- ''

# Test for missing data
left_join(areas_yrs_grps,
          by_area_yr_typ,
          by = c('area_cde' = 'area_code',
                 'year' = 'year',
                 'property_type' = 'property_type'
          )) -> grp_area_ppdata

View(grp_area_ppdata)

# # Excluding property type O = Other leaves only 11 segments with < 5 data points
# not_other <- by_area_yr_typ[by_area_yr_typ$property_type != "O", ]
# not_other[not_other$n_i < 5, ]



# Add column to calculate annual % price change in each area/type combo

# pct <- function(x) {
#     x/lag(x)
# }
# 
# full_data %>% group_by(area_code,
#                        property_type) %>% 
#     mutate(pct_change = (price / lag(price)) - 1) %>% 
#     
# 
# by_area_year_type <- by_area_year_type %>% 
#     mutate_each(funs(lag), lag(avg_price))
# 

by_area_type <- full_data %>% 
    group_by(area_code, 
             property_type,
             year) %>% 
    arrange(area_code,
            property_type,
            desc(year)) %>% 
    summarise(n = n(),
              avg_price = mean(price),
              sd = sd(price),
              threshold = quantile(price, 0.3)) %>% 
    mutate(pct = avg_price / lag(avg_price))
    # mutate(pct = avg_price / lead(avg_price))
    
by_area_type    
    
    
rev_sorted <- full_data %>% 
    arrange(desc(date_of_transfer)) %>% 
    group_by(area_code, 
             property_type,
             year) %>% 
    arrange(area_code,
            property_type,
            desc(year)) %>% 
    summarise(n = n(),
              avg_price = mean(price),
              sd = sd(price),
              threshold = quantile(price, 0.3))

rev_sorted
