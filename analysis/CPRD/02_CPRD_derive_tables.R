###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           Analyis of temporal and inter-practice variation
#
# File:     02_CPRD_derive_tables.R
# Date:     03/05/2019
# Task:     Calculate the necessary variables to perform the analysis using
#           the records extracted in 01_CPRD_extract_data.R
#
###########################################################################


subfolder <- "analysis/CPRD"

# Load the base settings and functions
source(file.path(subfolder, "00_init.R"))

library(forcats)
library(parallel)


# Load the extracted tables -----------------------------------------------

char(practices, patients, comorb, obese, smoke) %>% 
  walk(load_derived)




# Helper functions to identify all variables at certain time points -------

had_comorb <- function(dt, at_date, ord = sort(unique(dt$subclass))){
  # Select all patients that had a single given disease at or before a
  # given date.
  #
  # Args:
  #   dt - data.table containing all records for the disease
  #   at_date - date used as reference value
  #   ord - give the subclass hierarchy as character vector 
  #         (e.g. CKD stage 5 > CKD stage 4 > CKD stage 3 > ...)
  #
  # Result:
  #   data.table with a logical column named after the disease
  
  comorbidity <- unique(dt$comorbidity)
  
  if(length(comorbidity) != 1){
    stop("data.table must contain column disease with only one value")
  }
  
  dt <- 
    dt[eventdate <= at_date] %>% 
    .[, .(patid, ind = factor(subclass, ord))] %>% 
    .[order(patid, ind)] %>% 
    .[, .SD[1], by = patid]
  
  setnames(dt, "ind", comorbidity)
  unique(dt)
}


have_comorb <- function(dt, window, ord = sort(unique(dt$subclass))){
  # Select all patients that had a single given comorbitidty within 
  # a given time window
  #
  # Args:
  #   dt - data.table containing all records for the comorbidity
  #   window - a list of length two with two dates (in order)
  #
  # Result:
  #   data.table with a logical column named after the comorbidity
  
  comorbidity <- unique(dt$comorbidity)
  
  if(length(comorbidity) != 1){
    stop("data.table must contain column disease with only one value")
  }
  
  dt <- 
    dt[eventdate %between% window] %>% 
    .[, .(patid, eventdate, ind = factor(subclass, rev(ord)))] %>% 
    .[order(patid, eventdate, ind), .SD[.N], by = patid] %>% 
    .[, .(patid, ind = factor(ind, ord))]
  
  setnames(dt, "ind", comorbidity)
  unique(dt)
}





# Cut the data into monthly slices ----------------------------------------

eval_at_month <- function(year, month){
  
  # Define beginning of the year as reference date
  month_start <- make_date(year, month, 1)
  
  # Evaluate prevalence of the comorbidities at reference
  com_lvls <- list(
    asthma = "asthma",
    chd = "chd",
    ckd = paste0("ckd_stage_", 5:1),
    copd = "copd",
    dm = c("dm_type_1", "dm_type_2", "dm_unsp"),
    hf = "hf",
    pad = "pad",
    stroke = c("stroke", "tia")
  )
  
  com_month <- 
    comorb %>% 
    split(.$comorbidity) %>% 
    map2(com_lvls, had_comorb, at = month_start)
  
  # Obeseity at reference
  obese_month <- 
    obese[, .(patid, eventdate, 
              comorbidity = "obese", subclass = obese)] %>% 
    have_comorb(list(month_start %m-% years(5), month_start),
                c("severely obese", "obese", "non"))
  
  # Smoking at reference
  smoke_month <-
    smoke[, .(patid, eventdate, 
              comorbidity = "smoke", subclass = status)] %>% 
    have_comorb(list(month_start %m-% years(5), month_start), 
                c("smoke", "ex", "non"))
  
  ex <- 
    smoke[status %in% c("smoke", "ex") & eventdate < month_start, 
          .(patid = unique(patid))]
  smoke_month[ex, on = "patid", 
              smoke := ifelse(smoke == "non", 
                              which(levels(smoke) == "ex"), 
                              smoke)]
  
  
  # Identify all adult patients registered in that year
  cohort_month <-
    patients[month_start %between% list(enter_date, leave_date)]
  
  cohort_month[, age := ceiling(time_length(month_start - 
                                              birth_date, "years"))]
  cohort_month %<>% .[age %between% list(20, 110)]
  cohort_month[, c("birth_date", "death_date", 
                   "enter_date", "leave_date") := NULL]
  
  # Combine all columns into one table
  all_month <-
    c(list(cohort_month), com_month, list(obese_month, smoke_month)) %>% 
    reduce(merge, by = "patid", all.x = TRUE)
  all_month %<>% .[age %between% list(20, 110)]
  all_month[, age_cat := cut(age, c(19, 40, 60, 80, Inf), 
                                  c("21-40", "41-60", "61-80", "80+"))]
  all_month[, age := NULL]  
  
  
  # Consider missing values as no diagnoses for comorbidities and missing
  # for lifestyle factors (smoking and obesity)
  all_month[, names(com_lvls) := map(.SD, fct_explicit_na, "no"), 
            .SDcols = names(com_lvls)]
  all_month[, c("obese", "smoke") := map(.SD, fct_explicit_na, "missing"), 
            .SDcols = obese:smoke]
  
  
  all_month[]
}



# Run in a parallel setting
cl <- makeCluster(ceiling(detectCores() / 2))

ds <- c("patients", "comorb", "obese", "smoke")
fcts <- c("had_comorb", "have_comorb", "eval_at_month")
pkgs <- c("lubridate", "data.table", "purrr", "forcats", "magrittr")

clusterExport(cl, c(ds, fcts, "pkgs"))
loaded <- clusterEvalQ(cl, lapply(pkgs, library, character.only = TRUE))


# Each month for the study period and save the result
m <- expand.grid(month = 1:12, 
                 year = year(study_start):year(study_end))
m_slices <- parLapply(cl, purrr::transpose(m),
                      function(x) eval_at_month(x$year, x$month))
names(m_slices) <- str_c(m$year, m$month, sep = "_")

write_rds(m_slices, file.path(subfolder, der_dir, "m_slices.rds"), 
          compress = "gz")





# Calculate histogram for each month --------------------------------------

vars <- c("age_cat", "female", "imd", "asthma", "chd", "ckd", "copd",
          "dm", "hf", "pad", "stroke", "obese", "smoke")

discrete <- function(dt, cols){
  # Get the discrete joint distribution of columns.
  #
  # Args:
  #   dt - list of data.tables
  #   cols - the columns for the joint distribution
  #
  # Result:
  #   a list with key (column combinations) and each
  #   month's distribution
  
  possib <- 
    unique(tidyr::complete_(data = dt[[1]][, cols, with = FALSE], 
                            cols = cols)) %>% 
    as.data.table()
  
  dist <- dt %>% 
    map(~ .[, .(p = .N / nrow(.)), by = cols]) %>%
    map(~ .[possib, on = c(cols), .(p)]) %>% 
    map(~ .[is.na(p), "p" := 0])

  list(
    key = possib,
    dist = dist
  )
}


# Calculate for months and save
m_hist <- discrete(m_slices, vars)

write_rds(m_hist, file.path(subfolder, der_dir, "m_hist.rds"), 
          compress = "gz")








