library(data.table)
year_lag <- function(var, k) {
    if (k > 0) {
        # Bring past values forward k times
        return(c(rep(NA, k), head(var, -k)))
    } else {
        # Bring future values backward
        return(c(tail(var, k), rep(NA, -k)))
    }
}
x = data.table(a=1:10, 
               dte=sample(seq.Date(from=as.Date("2012-01-20"),
                                   to=as.Date("2012-01-30"), by=1),
                          10))

x[, L1_a:=panel_lag(a, 1)]  # This won't work correctly as `x` isn't keyed by date
x
setkey(x, dte)
x[, L2_a:=panel_lag(a, 2)]
x

DT <- data.table(x = sample(seq_len(20L)), 
                 category = sample(factor(LETTERS[1:4]), 20, replace = TRUE))
setkey(DT, category)
setorder(DT, category)
DT
DT[, 
   cum_prod := cumprod(x), 
   by = category]
DT
year_lag <- function(var, k) {
    if (k > 0) {
        # Bring past values forward k times
        return(c(rep(NA, k), head(var, -k)))
    } else {
        # Bring future values backward
        return(c(tail(var, k), rep(NA, -k)))
    }
}
map_int(seq_len(2018 - 1995), function(x) {
    
})

DT[, 
   lag_1 := year_lag(avg_price, 1), 
   by = .(outcde, prop_typ)]

DT$tax_year <- as.integer(DT$tax_year)
DT <- copy(by_outcde_yr_typ)
DT[, 
   `:=` ( diff_01 = avg_price / lag(avg_price, 01L), 
          diff_02 = avg_price / lag(avg_price, 02L), 
          diff_03 = avg_price / lag(avg_price, 03L), 
          diff_04 = avg_price / lag(avg_price, 04L), 
          diff_05 = avg_price / lag(avg_price, 05L), 
          diff_06 = avg_price / lag(avg_price, 06L), 
          diff_07 = avg_price / lag(avg_price, 07L), 
          diff_08 = avg_price / lag(avg_price, 08L), 
          diff_09 = avg_price / lag(avg_price, 09L), 
          diff_10 = avg_price / lag(avg_price, 10L), 
          diff_11 = avg_price / lag(avg_price, 11L), 
          diff_12 = avg_price / lag(avg_price, 12L), 
          diff_13 = avg_price / lag(avg_price, 13L), 
          diff_14 = avg_price / lag(avg_price, 14L), 
          diff_15 = avg_price / lag(avg_price, 15L), 
          diff_16 = avg_price / lag(avg_price, 16L), 
          diff_17 = avg_price / lag(avg_price, 17L), 
          diff_18 = avg_price / lag(avg_price, 18L), 
          diff_19 = avg_price / lag(avg_price, 19L), 
          diff_20 = avg_price / lag(avg_price, 20L), 
          diff_21 = avg_price / lag(avg_price, 21L), 
          diff_22 = avg_price / lag(avg_price, 22L), 
          diff_23 = avg_price / lag(avg_price, 23L)), 
   by = .(outcde, prop_typ)]

DT[, 
   diff_01 := avg_price / lag(avg_price, 01L), 
   diff_02 := avg_price / lag(avg_price, 02L), 
   diff_03 := avg_price / lag(avg_price, 03L), 
   diff_04 := avg_price / lag(avg_price, 04L), 
   diff_05 := avg_price / lag(avg_price, 05L), 
   diff_06 := avg_price / lag(avg_price, 06L), 
   diff_07 := avg_price / lag(avg_price, 07L), 
   by = .(outcde, prop_typ)]

lapply(list, function)