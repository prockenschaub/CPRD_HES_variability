###########################################################################
# Author:   Patrick Rockenschaub
#
# File:     03_other_variables.R
# Date:     25/09/2019
# Task:     Investigate the effect of individual variables on the temporal
#           variability plots
#
###########################################################################


library(plotly)
library(readr)
library(purrr)
library(stringr)
library(data.table)

source("00_variability_multisource.R")



# HES data ----------------------------------------------------------------
hes_by_age <- readRDS("make_plots/data/hes_by_age.rds")
hes_by_imd <- readRDS("make_plots/data/hes_by_imd.rds")
hes_by_sex <- readRDS("make_plots/data/hes_by_sex.rds")



# Run the plot with the chosen data ---------------------------------------

res <- hes_by_imd
font_size <- 20
scene <- list(camera = list(up = list(x = 0, y = 0, z = 1),
                            center = list(x = 0, y = 0, z = 0),
                            eye = list(x = -0.6, y = -2, z = 0.8)))

# Define the labels
y <- str_extract(names(res$Spos), "(?<=20)[0-9]{2}")
m <- str_extract(names(res$Spos), "(?<=_)[0-9]{1,2}")


# Define the colour and axis text size
periodcolor <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11,"Spectral"))(12)
ax <- list(titlefont = list(size = 24), tickfont = list(size = 18))

# Calculate a moving average
smoothed <- data.table(
  d1 = movavg(res$Vertices[, 1], 15),
  d2 = movavg(res$Vertices[, 2], 15),
  d3 = movavg(res$Vertices[, 3], 15)
)


# Plot the 3D plot

p <- plot_ly(as.data.frame(res$Vertices), x = ~d1, y = ~d2, z = ~d3,
             type = "scatter3d",
             mode = "text", text = str_c(y, m_abbr[m]), textposition = "middle",
             textfont = list(color = periodcolor[as.numeric(m)], size = font_size)) %>%
  add_trace(data = smoothed, x = ~d1, y = ~d2, z = ~d3, inherit = FALSE,
            type = "scatter3d",
            mode = "lines", line = list(dash = 'dash', color = "rgb(200, 200, 200)")) %>% 
  layout(scene = c(list(xaxis = c(list(title = 'Dimension 1'), ax),
                        yaxis = c(list(title = 'Dimension 2'), ax),
                        zaxis = c(list(title = 'Dimension 3'), ax)),
                   scene), showlegend = FALSE)

p

dpi <- 300
orca(p, "plots/tvm_plot.png", height = 4 * dpi, width = 6 * dpi)