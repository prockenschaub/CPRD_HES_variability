###########################################################################
# Author:   Patrick Rockenschaub
#
# File:     01_hes_heat_map.R
# Date:     25/09/2019
# Task:     Create Figures 2 & 3 - Temporal variability plots of CPRD and 
#           HES
#
###########################################################################

library(plotly)
library(readr)
library(purrr)
library(stringr)
library(data.table)

source("00_variability_multisource.R")



# HES data ----------------------------------------------------------------
res <- read_rds(file.path("make_plots", "data", "hes_all.rds"))

font_size <- 28
scene <- list(camera = list(up = list(x = 0, y = 0, z = 1),
                           center = list(x = 0, y = 0, z = 0),
                           eye = list(x = 0.3, y = 1.75, z = 0.5)))
ax <- list(titlefont = list(size = 24), tickfont = list(size = 18))
az <- ay <- ax



# CPRD data ---------------------------------------------------------------

age <- FALSE

# Without Age
cprd_files <- c("Gpd", "Spos", "DissimMatrix", "Vertices")
if(age) cprd_files <- str_c(cprd_files, "_age")

res <- 
  str_c( "cprd_", cprd_files, ".csv") %>% 
  set_names(cprd_files) %>% 
  map(~ fread(file.path("make_plots", "data", .)))

res$Gpd <- res$Gpd[[1]]
res$Spos <- set_names(res$Spos$spo, res$Spos$rn)
res$DissimMatrix <- as.matrix(res$DissimMatrix[, -1])
row.names(res$DissimMatrix) <- colnames(res$DissimMatrix)
res$Vertices <- as.matrix(res$Vertices[, -1])
row.names(res$Vertices) <- row.names(res$DissimMatrix)
res$Vertices[, 3] <- -res$Vertices[, 3]

# Flip coordinates for plotting (signs are artificial)
font_size <- 24
scene <- list(camera = list(up = list(x = 0, y = 0, z = 1),
                           center = list(x = 0, y = 0, z = 0),
                           eye = list(x = -0.3, y = -1.5, z = 1.4)))
ax <- az <- ay <- list(titlefont = list(size = 24), tickfont = list(size = 18), dtick = 0.01)


if(!age){
  ax <- c(ay, list(range = c(-0.04, 0.04)))
  res$Vertices[, 1] <- -res$Vertices[, 1]
}



# Run the plot with the chosen data ---------------------------------------


# Define the labels
y <- str_extract(names(res$Spos), "(?<=20)[0-9]{2}")
m <- str_extract(names(res$Spos), "(?<=_)[0-9]{1,2}")


# Define the colour and axis text size
periodcolor <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11,"Spectral"))(12)


# Calculate a moving average
smoothed <- data.table(
  d1 = movavg(res$Vertices[, 1], 15),
  d2 = movavg(res$Vertices[, 2], 15),
  d3 = movavg(res$Vertices[, 3], 15)
)


# Plot the 2D plot

ax_2d <- list(titlefont = list(size = 20), tickfont = list(size = 12))
p <- plot_ly(as.data.frame(res$Vertices), x = ~d1, y = ~d2,
             type = "scatter",
             mode = "text", text = str_c(y, m_abbr[m]), textposition = "middle",
             textfont = list(color = periodcolor[as.numeric(m)], size = 12)) %>%
  layout(scene = scene, showlegend = FALSE,
         xaxis = c(list(title = 'Dimension 1'), ax_2d),
         yaxis = c(list(title = 'Dimension 2'), ax_2d))

p


# Plot the 3D plot

p <- plot_ly(as.data.frame(res$Vertices), x = ~d1, y = ~d2, z = ~d3,
             type = "scatter3d",
             mode = "text", text = str_c(y, m_abbr[m]), textposition = "middle",
             textfont = list(color = periodcolor[as.numeric(m)], size = font_size)) %>%
  add_trace(data = smoothed, x = ~d1, y = ~d2, z = ~d3, inherit = FALSE,
            type = "scatter3d",
            mode = "lines", line = list(dash = 'dash', color = "rgb(200, 200, 200)")) %>% 
  layout(scene = c(list(xaxis = c(list(title = 'Dimension 1'), ax),
                        yaxis = c(list(title = 'Dimension 2'), ay),
                        zaxis = c(list(title = 'Dimension 3'), az)),
                   scene), showlegend = FALSE)

p

dpi <- 300
orca(p, "plots/tvm_plot.svg", height = 4 * dpi, width = 6 * dpi)


