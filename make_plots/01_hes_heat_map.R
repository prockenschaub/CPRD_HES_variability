###########################################################################
# Author:   Patrick Rockenschaub
#
# File:     01_hes_heat_map.R
# Date:     25/09/2019
# Task:     Create Figure 4 - ICD-10 heath map 
#
###########################################################################


library(magrittr)
library(stringr)
library(readr)
library(lubridate)
library(data.table)
library(EHRtemporalVariability)
library(plotly)


# Create a heat map of code frequencies -----------------------------------

# This section provides the code to create a heatmap of ICD-10 code
# frequencies from aggregated measures.
#
# For guidance on the needed datastructure, look at the example section of
# file msv_hes.R

probMaps <- read_rds(file.path("make_plots", "data", "hes_heat_map.rds"))

# Change the labels
labels <- read_csv(file.path("make_plots", "icd_card.csv"))
setDT(labels)
setkey(labels, icd)

rank <- probMaps$codes@support[[1]]
probMaps$codes@support[, 1] <- labels[rank, str_c("     ", icd, " - ", short)]

probMaps[["codes"]]@probabilityMap <- probMaps[["codes"]]@probabilityMap * 100

# Plot them for the ICD-10 codes
ax <- list(title = "", tickfont = list(color = "black", size = 24))
ay <- list(title = "ICD-10 code", autorange = "reversed",
           titlefont = list(color = "black", size = 32),
           tickfont = list(color = "black", size = 20),
           align = "left")

p <- plotDataTemporalMap(probMaps[["codes"]], 
                    startValue = 1, endValue = 20) %>% 
      layout(title = "", xaxis = ax, yaxis = ay)

p <- colorbar(p, nticks = 4, 
              tickfont = list(color = "black", size = 24), limits = c(0, 30), 
              ticksuffix = " %", showticksuffix = TRUE)
p

dpi <- 300
orca(p, "plots/hm_full.png", height = 3 * dpi, width = 6 * dpi)
orca(p, "plots/hm_full.svg", height = 3 * dpi, width = 6 * dpi)


         