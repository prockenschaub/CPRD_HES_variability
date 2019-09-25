###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           Analyis of temporal and inter-practice variation
#
# File:     04_CPRD_data_summary.R
# Date:     03/05/2019
# Task:     Calculate the data summaries presented in Table S of the 
#           supplementary material
#
###########################################################################



subfolder <- "analysis/CPRD"

# Load the base settings and functions
source(file.path(subfolder, "00_init.R"))



# Aggregate single percentages --------------------------------------------

m_hist <- read_rds(file.path(subfolder, der_dir, "m_hist.rds"))

aggr <- function(hist, var){
  # Get proportions for a single variable by aggregating the joint probabilities 
  
  map_df(hist$dist, ~ cbind(hist$key, .)[, .(p = sum(p)), by = c(var)], .id = "month")
}

condition <- "chd"

probs <- aggr(m_hist, condition) %>% copy()

probs[, year := as.integer(str_sub(month, end = 4L))]
probs[, month := as.integer(str_sub(month, start = 6L))]
probs[, time := make_date(year, month, 1)]

ggplot(probs[get(condition) != "no", .(p = sum(p)), by = time], aes(time, p, group = 1)) + 
  geom_line() + 
  theme_minimal()




# Get yearly summary measures ---------------------------------------------

m_slices <- readRDS(file.path(subfolder, der_dir, "m_slices.rds"))
m_slices <- m_slices[str_c(c(2001, 2005, 2010, 2015), "_7")]

map_int(m_slices, nrow)
map_df(m_slices, .id = "year", 
       ~ .[, .N, by = .(age_cat)][, .(age_cat, N, p = N / sum(N) * 100)])
map_df(m_slices, .id = "year", 
       ~ .[, .N, by = .(female)][, .(female, N, p = N / sum(N) * 100)])
map_dbl(m_slices, ~ .[, mean(as.numeric(imd))])

map_dbl(m_slices, ~ .[, mean(chd != "no") * 100])
map_dbl(m_slices, ~ .[, mean(hf != "no") * 100])
map_dbl(m_slices, ~ .[, mean(pad != "no") * 100])
map_df(m_slices, .id = "year", 
       ~ .[, .N, by = .(stroke)][, .(stroke, N, p = N / sum(N) * 100)])


