
sales_16 <- full_data[tax_year == 2016, 
                      .(price)]
library(ggplot2)
gg <- ggplot(sales_16, aes(x = price))

gg + 
    geom_histogram(binwidth = 25000) +
    theme_minimal()

sales_16[, 
         log_price := log(price)]

gg2 <- ggplot(sales_16, aes(x = log_price))

gg2 + geom_histogram(binwidth = 1, stat = "density") +
    theme_minimal() +
    stat_function(fun = dnorm, 
                  args = with(sales_16, c(mean = mean(log_price), 
                                          sd = sd(log_price))), 
                  colour = "blue")

gg2 + geom_density() +
    stat_function(fun = dnorm, 
                  args = with(sales_16, c(mean = mean(log_price), 
                                          sd = sd(log_price))), 
                  colour = "blue") +
    theme_minimal()

sales_16[, 
         exp_log_price := exp(log_price)]
