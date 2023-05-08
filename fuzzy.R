# Name: Saeesh Mangwani
# Date: 2020-05-04
# Description: Implementing a fuzzy rules based system for classifying risk using the FuzzyR package.

# ==== Loading libraries ====
library(tidyverse)
library(FuzzyR)

# Creating a new fuzzy inference system (fis) - most parameters are kept at
# default since the defualts are consistent with how Tara has defined her fis
fis <- newfis("hotspot_model2020")

# Add input and output variables to this fis
fis <- addvar(fis, 'input', 'diameter', c(0, 20))
fis <- addvar(fis, 'input', 'land_use', c(0, 1))
fis <- addvar(fis, 'input', 'month', c(1, 12))
fis <- addvar(fis, 'input', 'pressure', c(-115, 270))
fis <- addvar(fis, 'input', 'rainfall', c(0, 220))
fis <- addvar(fis, 'output', 'risk', c(1, 4))

# Adding membership functions (mfs) to each input variable (since the built-in
# function for adding mfs doesn't seem to be working, I wrote a custom function
# for doing it. Not this is not a robust function, i.e it does not contain any
# error checking and is merely a workaround to the lack of functionality in the
# package function)
addmf_custom <- function(fis, var_type, index, name, mf_type, params){
  # Getting a count of how many mfs are already present and using it to locate where to add the new mf
  num_mf <- length(fis[[var_type]][[index]]$mf) + 1
  # Creating an emtpy list at the location we want, that we'll then populate with values
  fis[[var_type]][[index]]$mf[[num_mf]] <- list()
  # Adding the name
  fis[[var_type]][[index]]$mf[[num_mf]]$name <- name
  # Adding the type
  fis[[var_type]][[index]]$mf[[num_mf]]$type <- mf_type
  # Adding the parameters
  fis[[var_type]][[index]]$mf[[num_mf]]$params <- params
  # returning the edited fis
  fis
}

# membership functions for diameter
fis <- addmf_custom(fis, 'input', 1, 'small', 'trapmf', c(-0.5, 3, 4, 5))
fis <- addmf_custom(fis, 'input', 1, 'medium', 'trapmf', c(4, 5, 6, 7))
fis <- addmf_custom(fis, 'input', 1, 'large', 'trapmf', c(6, 7, 20, 27))

# membership functions for landuse
fis <- addmf_custom(fis, 'input', 2, 'rural', 'trapmf', c(-0.36, -0.04, 0.1, 0.2))
fis <- addmf_custom(fis, 'input', 2, 'urban', 'trapmf', c(0.8, 0.9, 1.04, 1.36))

# membership functions for month
fis <- addmf_custom(fis, 'input', 3, 'very_dry', 'trapmf', c(-2.96, 0.56, 4, 4.4))
fis <- addmf_custom(fis, 'input', 3, 'dry', 'trapmf',  c(4.6, 5, 7, 7.4))
fis <- addmf_custom(fis, 'input', 3, 'wet', 'trapmf', c(7.6, 8, 12.44, 15.96))

# membership functions for pressure
fis <- addmf_custom(fis, 'input', 4, 'normal', 'trapmf', c(-25, 15, 75, 80))
fis <- addmf_custom(fis, 'input', 4, 'high', 'trapmf', c(75, 80, 285.4, 408.6))

# membership functions for rainfall
fis <- addmf_custom(fis, 'input', 5, 'very_dry', 'trapmf', c(-79.2, -8.8, 80, 85))
fis <- addmf_custom(fis, 'input', 5, 'dry', 'trapmf',  c(80, 85, 105, 110))
fis <- addmf_custom(fis, 'input', 5, 'wet', 'trapmf', c(195, 200, 228.8, 299.2))
fis <- addmf_custom(fis, 'input', 5, 'average', 'trapmf', c(105, 110, 195, 200))

# membership functions for risk
fis <- addmf_custom(fis, 'output', 1, 'low', 'trimf', c(-1.6, 0, 1))
fis <- addmf_custom(fis, 'output', 1, 'medium', 'trimf',  c(0.4, 1.6, 2.4))
fis <- addmf_custom(fis, 'output', 1, 'peak', 'trimf', c(3, 4, 5.6))
fis <- addmf_custom(fis, 'output', 1, 'high', 'trimf', c(1.6, 2.8, 4))


# Adding fuzzy logic rules
ruleList <- rbind(c(1,0,0,0,0,4,1,1), 
                  c(2,0,0,0,0,2,1,1),
                  c(3,0,0,0,0,1,1,1),
                  c(0,1,0,0,0,1,1,1),
                  c(0,2,0,0,0,2,1,1),
                  c(1,0,1,0,0,3,1,1),
                  c(1,0,2,0,0,4,1,1),
                  c(2,0,-3,0,0,2,1,1),
                  c(-3,0,3,0,0,2,1,1),
                  c(1,0,0,2,0,3,1,1),
                  c(-3,0,0,0,3,2,1,1),
                  c(-3,0,0,0,2,4,1,1),
                  c(-3,0,0,0,1,3,1,1))
fis <- addrule(fis, ruleList)

# Removing unnecessary objects and garbage collecting
rm(ruleList, addmf_custom)
gc()

