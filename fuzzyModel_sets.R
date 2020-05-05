# Name: Saeesh Mangwani
# Date: 2020-05-04
# Description: Implementing a fuzzy rules based system for classifying risk using the sets package.

# ==== Loading libraries ====
# A family of fundamental packages that assist with the manipulation, tidying and visualization of data in R
library(tidyverse)
library(sets)

# Loading pipe data (both sf and dataframe)
pipes <- read_csv("data/final_db.csv")
pipes_sdb <- read_sf("data/pipes_latlong.shp")

# Defining the range of the output, or the "universe" of values that the output set can take on
sets_options("universe", seq(from = 1, to = 4, by = 1))

# Setting up the fuzzy variables:
variables <- set(
  diameter = 
    fuzzy_variable(universe = seq(from = 0, to = 20, by = 0.1),
                   small = 
                     fuzzy_trapezoid(corners = c(-0.5, 3, 4, 5)),
                   medium = 
                     fuzzy_trapezoid(corners = c(4, 5, 6, 7)),
                   large = 
                     fuzzy_trapezoid(corners = c(6, 7, 20, 27))),
  land_use = 
    fuzzy_variable(universe = seq(from = 0, to = 1, by = 0.01),
                   rural = 
                     fuzzy_trapezoid(corners = c(-0.36, -0.04, 0.1, 0.2)),
                   urban = 
                     fuzzy_trapezoid(corners = c(0.8, 0.9, 1.04, 1.36))),
  month = 
    fuzzy_variable(universe = seq(from = 1, to = 12, by = 1),
                   very_dry = 
                     fuzzy_trapezoid(corners = c(-2.96, 0.56, 4, 4.4)),
                   dry = 
                     fuzzy_trapezoid(corners = c(4.6, 5, 7, 7.4)),
                   wet = 
                     fuzzy_trapezoid(corners = c(7.6, 8, 12.44, 15.96))),
  pressure = 
    fuzzy_variable(universe = seq(from = -115, to = 270, by = 0.5),
                   normal = 
                     fuzzy_trapezoid(corners = c(-25, 15, 75, 80)),
                   high = 
                     fuzzy_trapezoid(corners = c(75, 80, 285.4, 408.6))),
  rainfall = fuzzy_variable(universe = seq(from = 0, to = 220, by = 0.5),
                            very_dry = 
                              fuzzy_trapezoid(corners = c(-79.2, -8.8, 80, 85)),
                            dry = 
                              fuzzy_trapezoid(corners = c(80, 85, 105, 110)),
                            wet = 
                              fuzzy_trapezoid(corners = c(195, 200, 228.8, 299.2)),
                            average = 
                              fuzzy_trapezoid(corners = c(105, 110, 195, 200))),
  risk = fuzzy_variable(universe = seq(from = 1, to = 4, by = 1),
                        low = 
                          fuzzy_triangular(corners = c(-1.6, 0, 1)),
                        medium = 
                          fuzzy_triangular(corners = c(0.4, 1.6, 2.4)),
                        peak = 
                          fuzzy_triangular(corners = c(3, 4, 5.6)),
                        high = 
                          fuzzy_triangular(corners = c(1.6, 2.8, 4)))
  )

# Degining the rules that will be used for fuzzy matching the inputs to the output risk score
rules <- set(
  fuzzy_rule(diameter %is% small, risk %is% high),
  fuzzy_rule(diameter %is% medium, risk %is% medium),
  fuzzy_rule(diameter %is% large, risk %is% low),
  fuzzy_rule(land_use %is% rural, risk %is% low),
  fuzzy_rule(land_use %is% urban, risk %is% medium),
  fuzzy_rule(diameter %is% small && month %is% very_dry, risk %is% peak),
  fuzzy_rule(diameter %is% small && month %is% dry, risk %is% high),
  fuzzy_rule(diameter %is% medium && !(month %is% wet), risk %is% medium),
  fuzzy_rule(!(diameter %is% large) && month %is% wet, risk %is% medium),
  fuzzy_rule(diameter %is% small && pressure %is% high, risk %is% peak),
  fuzzy_rule(!(diameter %is% large) && rainfall %is% wet, risk %is% medium),
  fuzzy_rule(!(diameter %is% large) && rainfall %is% dry, risk %is% high),
  fuzzy_rule(!(diameter %is% large) && rainfall %is% very_dry, risk %is% peak)
)

# Combing the above into a system, and printing summaries and visualizations to understand the structure of this system
system <- fuzzy_system(variables, rules)
print(system)
plot(system) ## plots variables

# Testing the system through providing input to get inferences
fi <- fuzzy_inference(system, values = list(diameter = 12, land_use = 1, month = 3, pressure = 270, rainfall =  270), implication = "minimum")

# plotting resulting fuzzy set
plot(fi)

## defuzzifying to get a risk score using the centroid method
gset_defuzzify(fi, "centroid")

## reset universe
sets_options("universe", NULL)
