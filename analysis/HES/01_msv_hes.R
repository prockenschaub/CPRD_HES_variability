

library(data.table)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(ggforce)

source("00_variability_multisource.R")

# Example -----------------------------------------------------------------

# This section provides an artificial example that is roughly based on the 
# real structure of Vincent's dataset. It is meant to show how the code 
# should be used and enable a quick adaptation to the actual dataset.

set.seed(1234)

# Define variables
codes <- str_c("I", 20:29)
year <- 2002:2015
month <- 1:12
age <- c("0-20", "20-40", "40-60", ">60")
sex <- c("male", "female")
imd <- 1:5

# Create some dependencies between them an admissions
intercept <- 4.5
y_coefs <- 2:15 / 20
m_coefs <- abs(1:12 - 6) / 12
a_coefs <- c(0.1, 0, 0, 0.2)
s_coefs <- c(0, -0.1)
i_coefs <- 2:(-2) / 10

names(y_coefs) <- year
names(a_coefs) <- age
names(s_coefs) <- sex

# Two frequencies for each code, before and after change
codes_1 <- runif(length(codes), min = -1, max = 1)
codes_2 <- runif(length(codes), min = -1, max = 1)
names(codes_1) <- names(codes_2) <- codes


# Simulate numbers
data <- mget(c("year", "month", "age", "sex", "imd", "codes")) %>% 
  expand.grid() %>% 
  as.data.table()

data[, eta := intercept + y_coefs[as.character(year)] + m_coefs[month] +  
              a_coefs[age] +s_coefs[sex] + i_coefs[imd]]
data[, eta := eta + if_else(year < 2012, codes_1[codes], codes_2[codes])]
data[, N := rpois(.N, exp(eta))]


data[, eta := NULL]


# Calculate variability metrics -------------------------------------------

# This section provides the basic code needed to run the variability
# analysis. All further additional analysis will depend on the results of 
# this section. 

# Ideally, the output of this section is stored and can be shared for 
# further analysis (if possible by any privacy restrictions). At this point
# data is completely aggregated on a monthly level and should be 
# sufficiently anonymised to share. 


# Get monthly distributions
data[, p := N / sum(N), by = .(year, month)]

# Calculate metrics
res <- data %>% 
  split(str_c(.$year, "_", .$month)) %>% 
  var_from_list()

# Plot the results
y <- str_extract(names(res$Spos), "(?<=20)[0-9]{2}")
m <- str_extract(names(res$Spos), "(?<=_)[0-9]{1,2}")

base_var_plot(res) + 
  avg_line(res, n = 15) + 
  geom_text(aes(label = str_c(y, m_abbr[m]),
                colour = as.numeric(m))) + 
  labs(x = "Dimension 1", y = "Dimension 2") + 
  coord_cartesian(ylim = c(-0.1, 0.1), xlim = c(-0.15, 0.2))


# Save the results
write_rds(res, "var_results.rds")



