# Data-driven discovery of changes in clinical code usage over time

This is the R code used to analyse the CPRD-HES data for paper "Data-driven discovery of changes in clinical code usage over time: a case-study on changes in cardiovascular disease recording in two English electronic health records databases (2001-2015)
" available as preprint on medRxiv ((https://doi.org/10.1101/19006098)[https://doi.org/10.1101/19006098]). 

## Data structure

All files located in the folder `analysis` were used to calculate the temporal variability metrics for CPRD and HES. These procedures were performed in a data safe haven, hence not all files needed to replicate the analysis are available. However, the presented code should allow readers to infer the methods used to obtain the results.

The folder `make_plots` contains code to create the plots based on the analysis performed in the data safe haven. The actual dissimilarity matrices are availabe and code can be rerun to create the plots in the publication.
