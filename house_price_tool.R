# Import libraries for data wrangling

library(tidyr)
library(dplyr)
library(data.table)
library(readr)
library(reshape2)
library(purrr)
library(lubridate)
library(stringr)
library(magrittr)

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
    if(!dir.exists("/data/")) {
        dir.create("/data/")
    }
    setwd("/data")
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
                    function(x) fread(x,
                                      sep = ",",
                                      header = F, 
                                      col.names = c('tuid', 'price', 
                                                    'date_of_transfer', 
                                                    'postcde', 'prop_typ', 
                                                    'old_new', 'duration', 
                                                    'paon', 'saon', 'street', 
                                                    'locality', 'town', 
                                                    'district', 'county', 
                                                    'ppd_type', 
                                                    'rec_status')))
    
    full_data <- rbindlist(datalist)
    
    rm(datalist)
    
    # Clean up the data: change certain columns to factors, set names for columns, 
    # and reorder them.
    full_data <- full_data[
        , 
        prop_typ := as.factor(prop_typ)
        ][
        , 
        old_new := as.factor(old_new)
        ][
            ,
            duration := as.factor(duration)
        ]
    
    full_data %>%
        separate(postcde, 
                 into = c("outcde", "incde"), 
                 sep = "\\ ",
                 extra = "merge",
                 fill = "right") %>% 
        select(price, date_of_transfer, 
               outcde, 
               incde,
               prop_typ, 
               everything()) -> full_data
    
    # Clean and standardise the dates, add column for year
    full_data <- full_data[, 
                           date_of_transfer := as_date(date_of_transfer)][
                               , 
                               year := year(date_of_transfer)
                           ]
    
    full_data %>%
        select(price, 
               date_of_transfer, 
               year, 
               outcde, 
               incde, 
               prop_typ, 
               everything()) -> full_data
    
    # Write out full_data to rds to save reprocessing
    setwd('..')
    saveRDS(full_data, './data/full_data.rds')
}

# If full_data is not already in memory the ifelse will first try to read in 
# rds file with the full_data data.table, otherwise will run the 
# create_full_data function.
if(!exists('full_data')) {
    ifelse(file.exists('./data/full_data.rds'),
           full_data <- as.data.table(readRDS('./data/full_data.rds')),
           create_full_data())
}

# Group the data by outcode, year, and property type, then summarise with a few 
# key stats
if(!exists('by_outcde_yr_typ')) {
    ifelse(file.exists('/data/by_outcde_yr_typ.rds'),
           by_outcde_yr_typ <- as.data.table(readRDS('/data/by_outcde_yr_typ.rds')),
           by_outcde_yr_typ <- full_data[, 
                                         
                                         .(.N, 
                                           avg_price = mean(price),
                                           median = quantile(price, .50),
                                           sd = sd(price),
                                           q25 = quantile(price, .25),
                                           q20 = quantile(price, .20),
                                           q15 = quantile(price, .15),
                                           q10 = quantile(price, .10),
                                           q05 = quantile(price, .05)), 
                                         
                                         keyby = .(outcde, prop_typ, year)])
}

# Write out the data.table to rds
if(!file.exists('~/housing_data/data/by_outcde_yr_typ.rds')) {
    saveRDS(by_outcde_yr_typ, '~/housing_data/data/by_outcde_yr_typ.rds')
}

# Check how many groups have at least 10 data points
paste0(round((100 * nrow(by_outcde_yr_typ %>% 
                     filter(N > 10)) / nrow(by_outcde_yr_typ)),
            2),
       "%")


# Combine the Ordnance Survey postcode list csv files to get full list of all 
# UK outcodes.
fils <- list.files('./data/OS_data', 
                   pattern = '.csv$', 
                   full.names = TRUE)

pcdes <- rbindlist(lapply(fils, 
                          fread))[, 
                                  .(pcde = paste(str_extract(V1, 
                                                             '^[A-z]{1,2}\\d{1,2}[A-z]?'),
                                                 str_extract(V1, 
                                                             '\\d[A-z]{2}$')),
                                    outcde = str_extract(V1, 
                                                         '^[A-z]{1,2}\\d{1,2}[A-z]?'),
                                    incde = str_extract(V1, 
                                                        '\\d[A-z]{2}$'))]


# Test whether all pcdes have required format
min(str_detect(pcdes$pcde, "[A-z]{1,2}\\d{1,2}[A-z]? \\d[A-z]{2}")) == 1

# Test whether all outcdes have required format
min(str_detect(pcdes$outcde, "[A-z]{1,2}\\d{1,2}[A-z]?")) == 1

# Drop cols and duplicates from pcdes to save memory
outcdes <- rbindlist(list(list(''), 
                          unique(pcdes[, .(outcde)])))

rm(pcdes)

# Prepare all_outcdes for binding with year and type data
outcdes_yrs_typs <- CJ(outcde = outcdes$outcde, 
                       year = 2015:2016,
                       prop_typ = c('D', 'F', 'O', 'S', 'T'))


# Left Join full list of outcodes with the price paid data, see which have no 
# data. Add logical for Scottish pcdes.
scot_area_cdes <- c('AB', 'DD', 'DG', 'EH', 'FK', 'G', 'HS', 'IV',
                    'KA', 'KW', 'KY', 'ML', 'PA', 'PH', 'TD', 'ZE')
scot_outcde_regx <- paste0('^',
                           scot_area_cdes,
                           '[0-9]{1,2}',
                           collapse = '|')
setkey(outcdes_yrs_typs, outcde, prop_typ, year)
setkey(by_outcde_yr_typ, outcde, prop_typ, year)
all_ppdata <- merge(outcdes_yrs_typs, by_outcde_yr_typ, all.x = T)[
    ,
    scottish := (grepl(scot_outcde_regx, outcde))
]
sapply(all_ppdata, function(x) {max(is.na(x))})
sapply(all_ppdata, function(x) {sum(is.na(x))})

# Separate Scottish data
main_ppdata <- all_ppdata[scottish == FALSE]
scot_ppdata <- all_ppdata[scottish == TRUE]

# What proportion of transactions occur in areas with low/no data?
data_quality_summ <- main_ppdata[
    , 
    .(N = .N,
      N10_more = sum(N >= 10, na.rm = T),
      N05_more = sum(N >= 5, na.rm = T),
      no_data = sum(is.na(N)))
]
main_ppdata[, 
            .N, 
            keyby = .(low_data = (N < 10 & !is.na(N)),
                      no_data = is.na(N))
            ]



# Use first part of outcode to generate larger n groups that can be used to 
# examine changes in price over time

# full_data <- full_data %>% 
#     mutate(area_code = str_extract(outcode, "[A-Z]{1,2}")) %>% 
#     select(price, 
#            date_of_transfer, 
#            year, 
#            area_code, 
#            outcode, 
#            property_type, 
#            incode, 
#            everything())
# 
# by_area_yr_typ <- full_data %>%
#     group_by(area_code, 
#              property_type,
#              year) %>% 
#     summarise(n = n(),
#               avg_price = mean(price), 
#               sd = sd(price), 
#               q05 = quantile(price, .05),
#               q10 = quantile(price, .10),
#               q15 = quantile(price, .15), 
#               q20 = quantile(price, .20),
#               q25 = quantile(price, .25),
#               q50 = quantile(price, .50),
#               q75 = quantile(price, .75))
# 
# # Create an areas_yrs_grp table to check for missing data
# outcdes_yrs_typs %>% 
#     mutate(area_cde = str_extract(outcde, "[A-z]{1,2}")) %>% 
#     select(area_cde, year, property_type) %>% 
#     distinct(.) -> areas_yrs_grps
# 
# # Set NAs to empty string
# areas_yrs_grps[is.na(areas_yrs_grps$area_cde), 1] <- ''
# 
# # Test for missing data
# left_join(areas_yrs_grps,
#           by_area_yr_typ,
#           by = c('area_cde' = 'area_code',
#                  'year' = 'year',
#                  'property_type' = 'property_type'
#           )) -> grp_area_ppdata
# 
# View(grp_area_ppdata)

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

# by_area_type <- full_data %>% 
#     group_by(area_code, 
#              property_type,
#              year) %>% 
#     arrange(area_code,
#             property_type,
#             desc(year)) %>% 
#     summarise(n = n(),
#               avg_price = mean(price),
#               sd = sd(price),
#               threshold = quantile(price, 0.3)) %>% 
#     mutate(pct = avg_price / lag(avg_price))
#     # mutate(pct = avg_price / lead(avg_price))
#     
# by_area_type    
#     
#     
# rev_sorted <- full_data %>% 
#     arrange(desc(date_of_transfer)) %>% 
#     group_by(area_code, 
#              property_type,
#              year) %>% 
#     arrange(area_code,
#             property_type,
#             desc(year)) %>% 
#     summarise(n = n(),
#               avg_price = mean(price),
#               sd = sd(price),
#               threshold = quantile(price, 0.3))
# 
# rev_sorted
