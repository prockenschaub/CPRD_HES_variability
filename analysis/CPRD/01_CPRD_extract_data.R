###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           Analyis of temporal and inter-practice variation
#
# File:     01_CPRD_extract_data.R
# Date:     03/05/2019
# Task:     Extract all the necessary tables from the database
#
###########################################################################


# NOTE: This code uses functions defined in 00_base_tables.R, which load
#       CPRD data from a Microsoft SQL database. The tables in the database
#       follow CPRD's standard data format.
#
#       For more information, please contact the corresponding author.


# Load only if not called before
subfolder <- "analysis/CPRD"
  
# Load the base settings and functions
suppressMessages({
  source(file.path(subfolder, "00_init.R"))
})


first_record <- function(dt){
  # Get the earliest record for each patient in a data.table
  #
  # Args:
  #   dt - data.table with patient id and eventdate
  #
  # Result:
  #   a data.table with one row per patient, which is the earliest event
  
  
  dt[order(patid, eventdate), .SD[1], by = patid]
}



# Select practices --------------------------------------------------------
## @knitr practices
#+ practices, include = FALSE

# Obtain information on when practices provided data
def_practice <- 
  practice_db() %>% 
  filter(
    linked == 1L, 
    uts < study_end, 
    lcd > study_start) %>% 
  compute(name = "practices_ms")

practices <- collect_dt(def_practice, convert = TRUE)



# Select patient population -----------------------------------------------
## @knitr patients
#+ patients, include = FALSE

# Define start and end dates
def_patients <-
  study_population_db(link = TRUE) %>% 
  in_date(study_start, study_end) %>% 
  semi_join(def_practice, by = "pracid") %>% 
  left_join(imd_db(), by = c("patid", "pracid"))

# Apply exclusion criteria
def_patients <-
  def_patients %>% 
  filter(
    !is.na(birth_date), 
    !is.na(female), 
    !is.na(imd)
  ) %>% 
  select(patid, pracid, female, imd, birth_date, 
         death_date, enter_date, leave_date)

def_patients <- compute(def_patients, name = "patients_ms")

patients <- collect_dt(def_patients, convert = TRUE)



# Obtain comorbidities ----------------------------------------------------
## @knitr comorbs
#+ comorbs, include = FALSE

# Use all other comorbidity codes
codes_qof <- 
  codes_qof_db() %>% 
  collect_dt()


# Select all records with one of those codes in the study population
# NOTE: exclude COPD (by definition) and CKD stage 1&2 (unreliable coding)
comorb <- 
  qof_db() %>% 
  records_db(codes_qof) %>% 
  semi_join(def_patients, by = "patid") %>% 
  filter(!is.na(eventdate)) %>% 
  select(-num_rows) %>% 
  collect_dt(convert = TRUE)


# Add the disease labels
comorb[codes_qof, on = "medcode", comorbidity := comorbidity]
comorb[codes_qof, on = "medcode", subclass := subclass]


# Obtain obesity status ---------------------------------------------------
## @knitr obese
#+ obese, include = FALSE

# Get all explicit obesity diagnoses
obese_diag <- 
  obese_db() %>% 
  semi_join(def_patients, by = "patid") %>% 
  collect_dt(convert = TRUE)

obese_codes <- codes_obese_db() %>% collect_dt()
obese_codes[, obese := ifelse(str_detect(description, "40"), 
                              "severely obese", "obese")]


# Get additional BMI measurements (BMI, weight, height)
bmi <- 
  bmi_db() %>% 
  semi_join(def_patients, by = "patid") %>% 
  collect_dt(convert = TRUE)

bmi[, c("weight", "bmi", "height") := map(.SD, as.numeric), 
    .SDcols = weight:height]

setorder(bmi, patid, eventdate)

# Carry the last height observation forward for each patient
bmi[, height := .(if(is.na(height[1])) c(-Inf, height[-1]) 
                   else  height), by = patid]
bmi[, height := zoo::na.locf(height), by = patid]
bmi[height < 1.3, height := NA]

# When there is no BMI value but weight and (forward-carried) height,
# compute BMI under the assumption that height doesn't change too much
setorder(bmi, patid, eventdate, bmi, na.last = TRUE)
bmi %<>% .[, .SD[1], by = .(patid, eventdate)]
bmi[is.na(bmi), bmi := weight / (height ** 2)]


# Combine both to a measure of obesity
obese <- 
  rbind(
    obese_diag[obese_codes, on = "medcode", nomatch = 0, 
               .(patid, eventdate, obese)],
    bmi[!is.na(bmi), .(patid, eventdate, 
          obese = case_when(bmi >= 40 ~ "severely obese", 
                            bmi >= 30 ~ "obese",
                            TRUE ~ "non"))]
  ) %>% unique()



# Obtain smoking status ----------------------------------------------------
## @knitr smoke
#+ smoke, include = FALSE

smoke <- 
  smoke_db() %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(-adid, -data2, -data3, -num_rows) %>% 
  collect_dt(convert = TRUE)

codes_smoke <- codes_smoke_db() %>% collect_dt()

# Use CALIBER smoking algorithm 
# (www.caliberresearch.org/portal/show/smoking_status_gprd)
smoke[codes_smoke, on = "medcode", status := smoke]
smoke[enttype == 4, 
      status := case_when(data1 == "1" & status == "non" ~ "non & smoke",
                          data1 == "1" & status == "ex"  ~ "ex & smoke",
                          data1 == "1" ~ "smoke",
                          data1 == "2" & status == "smoke" ~ "non & smoke",
                          data1 == "2" & status == "ex" ~ "ex",
                          data1 == "2" ~ "non",
                          data1 == "3" & status == "smoke" ~ "ex & smoke",
                          data1 == "3" ~ "ex",
                          TRUE ~ status)]

# Change the ambivalent codes in favour of non-smoking (this is justified,
# as a) it is more conservative and b) there are very general codes that
# are currently counted as smoking, but those could easily just be used 
# with indicating a smoking status (e.g. 137..00 Tobacco consumption)
smoke[status == "non & smoke", status := "non"]
smoke[status == "ex & smoke", status := "ex"]

smoke[, c("enttype", "data1") := NULL]





# Save all derived datasets -----------------------------------------------
## @knitr save
#+ save, include = FALSE

mget(c("practices", "patients", "comorb", "obese", "smoke")) %>% 
  walk2(., names(.), save_derived, compress = "gz")

