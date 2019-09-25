###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           COPD analysis
#
# File:     00_init.R
# Date:     07/11/2018
# Task:     Initialise the workspace for a clean new analysis of antibiotic
#           prescribing in COPD patients (i.e. remove previous objects, 
#           load libraries, etc.)
#
###########################################################################


# Load the general environment (only if not called before)
if(exists(".conn")){
   disconnect_db()
}

# Load the base settings and functions
suppressMessages({
  source(file.path("analysis/CPRD", "00_init.R")) # TODO: extract from data safe haven
  source(file.path("analysis/CPRD", "00_basic_tables.R"))
  source(file.path("analysis/CPRD", "00_code_lists.R"))
})

# Load local functions
subfolder <- "analysis/CPRD"
source(file.path(subfolder, "00_functions.R"))

# Set path to store the derived datasets
der_dir <- "01_derived"
res_dir <- "02_results"

# Set default values
study_start <- ymd("2001-01-01")
study_end <- ymd("2015-12-31")
