# variability_multisource.R
#
# Obtains the multi-source stability metrics Global Probabilistic Deviation (GPD) and
# Source Probabilistic Outlyingness (SPOs) among the provided set of probability distributions.
# Calculated according to the method described in:
#
#   [C S?ez, M Robles, JM Garc?a-G?mez. Stability metrics for multi-source biomedical
#   data based on simplicial projections from probability distribution distances.
#   Statistical Methods in Medical Research (2017) 26-1:312-336]
#
# Input:
#
# probabilities   m-by-n matrix containing the probability mass of n data sources on m distribution bins
#
# Output:
# List containing the following results:
#
# Gpd           the value of the Global Probabilistic Deviation metric, where 0 means equal distributions and 1 means non-overlapping
# Spos          the values of Source Probabilistic Outlyingness for each data source, where 0 means equal to central tendency and 1 completelly non-overlapping, this can be used as the input for a colormap
# DissimMatrix  a n-by-n dissimilarity matrix of the Jensen-Shannon probability distribution distances among the provided n probability distributions
# Vertices      a n-by-d matrix (where d = n-1) containing the d-dimensional coordinates of each data source in the projected probabilistic space conserving their dissimilarities, coordinates are sorted by explained variance
#
#
# VERSION 1.1 - 2017/11/23
#
# COPYRIGHT (C) NOTICE:
# This function has been developed by the Biomedical Data Science Lab (BDSLab),
# Instituto Universitario de Tecnolog?as de la Informaci?n y Comunicaciones (ITACA)
# Universitat Polit?cnica de Val?ncia (UPV), Spain
# Registered in the Intellectual Property Rights registry of the UPV.
# All Rights Reserved.
# 
# Contact info: Carlos S?ez <carsaesi@upv.es>
#
# *This function can only be used with academic purposes.*
# *If you use this function please cite the publication referred above [S?ez et al 2017].*



# Convenience functions (added by Patrick Rockenschaub) -------------------

var_from_list <- function(p_list){
  # Transform a a list of probability data.frames into a matrix before 
  # calculating the variability measures.
  #
  # Args:
  #   p_list - a list with one element per source; the element must be a 
  #            a data.frame with a column p that contains the probabilities
  #
  # Results:
  #   see variability_multisource()
  
  p_list %>% 
    map_df("p") %>% 
    as.matrix() %>% 
    variability_multisource()
}

var_from_matrix <- function(p_matrix){
  # Wrapper around variability_multisource for k = ns - 1.
  #
  # Args:
  #   p_matrix - matrix with the probabilities for one source/time 
  #              batch in each column
  #
  # Results:
  #   see variability_multisource()
  
  variability_multisource(p_matrix)
}



# Plot monthly data
movavg <- function(x, n){
  # Calculate the moving average of a numerical vector.
  #
  # Args:
  #   x - numerical vector
  #   n - window size as integer
  #
  # Result:
  #   vector of length x - n + 1 with all moving averages
  
  ma <- vector(length(x) - n + 1, mode = "numeric")
  
  for(i in seq_along(ma)){
    ma[i] <- mean(x[i:(i+n-1)])
  }
  
  ma
}

avg_line <- function(var_obj, dims = c(1, 2)){
  # Create a smoothed line based on a moving average of length 5
  # 
  # Args:
  #   var_obj - the result of a variability_multisource() call
  #   dims - the dimensions for which the average should be 
  #          calculated; must match the dimensions that are plotted
  #
  # Results:
  #   a geom_segment object that defines the average line
  
  res_avg <- data.table(
    x = movavg(var_obj$Vertices[, dims[1]], 5), 
    y = movavg(var_obj$Vertices[, dims[2]], 5)
  )
  res_avg[, xend := shift(x, type = "lead")]
  res_avg[, yend := shift(y, type = "lead")]
  
  geom_segment(data = res_avg[-nrow(res_avg)], linetype = 2,
               aes(x, y, xend = xend, yend = yend))
}  


base_var_plot <- function(var_obj, dims = c(1, 2)){
  # Define a basic panel for a variability plot
  #
  # Args:
  #   var_obj - the result of a variability_multisource() call
  #   dims - the dimensions that should be plotted
  
  ggplot(data = NULL, aes(x = var_obj$Vertices[, dims[1]], 
                          y = var_obj$Vertices[, dims[2]])) + 
    geom_circle(data = NULL, aes(x = NULL, y = NULL, x0 = 0, y0 = 0, 
                                 r = d1R(length(var_obj$Spos) - 1)), 
                colour = "black") + 
    geom_point(data = data.table(z = 1), x = 0, y = 0, size = 3, colour = "black") + 
    scale_color_distiller(palette = "Spectral") + 
    guides(colour = FALSE) + 
    theme_minimal()
}


# Define the monthly abbrevations
m_abbr <- c("J", "F", "M", "A", "m", "j", "x", "a", "S", "O", "N", "D")
names(m_abbr) <- 1:12



# Original code created by Carlos Saez ------------------------------------

variability_multisource <- function(probabilities, k = ns-1) {
  # Perform an analysis of the multisource variabilities of a dataset.
  #
  # Args: 
  #   probabilities - matrix with the probabilities for one source/time 
  #                   batch in each column
  #   k - number of dimensions for the multidimensional scaling
  #
  # Results:
  #   a list including the GPD, the individual SPOs, the dissimilarity 
  #   matrix and the vertices of the geometric embedding.
  
  
  ns = ncol(probabilities);

  # Calculate the dissimilarity matrix based on Jensen-Shannon divergence
  dissimMatrix = matrix(data=0,nrow=ns,ns)
  for(i in 1:ns){
    for(j in 2:ns){
      dissimMatrix[i,j] = sqrt(jsdiv(probabilities[,i],probabilities[,j]))
      dissimMatrix[j,i] = dissimMatrix[i,j]
    }
  }
  
  # Embed into a Euclidean space
  vertices <- cmdscale(dissimMatrix, eig=FALSE, k=k)
  
  # Calculate variability metrics (GPD and SPOs)
  c = colSums(vertices)/ns
  cc = matrix(rep(c, ns),nrow=ns,byrow=TRUE)
  cc2 = vertices-cc
  dc = apply(cc2, 1, norm, type="2")
  
  gpd = mean(dc)/distc(ns - 1)
  spos = dc/(1-(1/ns))
  
  # Name all elements by source/dimension
  names(spos) <- colnames(probabilities)
  colnames(dissimMatrix) <- rownames(dissimMatrix) <- colnames(probabilities)
  rownames(vertices) <- colnames(probabilities)
  colnames(vertices) <- paste0("d", 1:ncol(vertices))

  # Return results
  list(Gpd = gpd, 
       Spos = spos, 
       DissimMatrix = dissimMatrix, 
       Vertices = vertices)
}

jsdiv <- function(p, q){
  # Calculate the Jensen-Shannon divergence between two (discretised)
  # probability distributions.
  #
  # Args:
  #   p, q - two equal length vectors with probabilities
  #
  # Results:
  #   a numeric vector of length one
  
  m <- log2(0.5 * (p + q))
  
  0.5 * (sum(p * (log2(p) - m), na.rm = TRUE) + 
         sum(q * (log2(q) - m), na.rm = TRUE))
}

distc <- function(D){
  gamma = acos(-1/D)
  temp = sin((pi-gamma)/2)/sin(gamma)
  temp[D==1] = 0.5
  temp
}


d1R <- function(D) { 
  # The distance of a vertex to its centroid in a 1-regular simplex
  # in D dimensions. A 1-regular simplex is a simplex for which all
  # distances between the vertices are 1.
  #
  # Args: 
  #   D - number of dimensions
  #
  # Result:
  #   a length 1 vector with the distance
  
  1 / (2 * sin(acos(-1 / D) / 2)) 
}


