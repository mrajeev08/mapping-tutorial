# ------------------------------------------------------------------------------------------------ #
#' Make fake spatial serodata
#' replicating data from Chennai Serology Workshop Nov 2018, simulating for sharing on github 
#' so that all data is randomly generated
# ------------------------------------------------------------------------------------------------ #

# Libraries
library(raster)
library(tidyverse)
library(magrittr)
library(rgdal)

# Get data sources (world pop + shapefiles) --------------------------------------------------
# these two are local files (large!) but can be downloaded from World Pop
india_pop <- raster("~/Google Drive/IND_ppp_2015_adj_v2/IND_ppp_2015_adj_v2.tif")
india_births <- raster("~/Google Drive/IND_births_pp_v2_2015/IND_births_pp_v2_2015.tif")

# Aggregate india world pop so th at it's more manageable (takes ~ 30 min)
india_pop <- aggregate(india_pop, fact = 10, fun = sum, na.rm = TRUE)
writeRaster(india_pop, "data/india_pop.tif", overwrite = TRUE)
india_births <- aggregate(india_births, fact = 10, fun = sum, na.rm = TRUE)
writeRaster(india_births, "data/india_births.tif", overwrite = TRUE)

# Get India admin 2 shapefile using Malaria Atlas R package
library(malariaAtlas)
india_shp <- malariaAtlas::getShp(country = "India", admin_level = "admin2")
writeOGR(india_shp, "data/India", "India", driver = "ESRI Shapefile")

# Getting punjab and assam admin 2 population sizes
punjab <- subset(india_shp, name_1 == "Punjab")
assam <- subset(india_shp, name_1 == "Assam")

# assam
assam_pop <- crop(india_pop, assam) 
assam_pop <- mask(assam_pop, assam) 

# punjab
punjab_pop <- crop(india_pop, punjab) 
punjab_pop <- mask(punjab_pop, punjab) 

# Extract pops
assam <- extract(assam_pop, assam, fun = sum, na.rm = TRUE, sp = TRUE) 
punjab <- extract(punjab_pop, punjab, fun = sum, na.rm = TRUE, sp = TRUE)

# Generating simulated data to match data used by participants
# Need the following variables: 
# AGE (decimal 0 - 100), SITEID (Punjab/Assam)
# REVQUAL_M (Postive/Negative), REVQUAL_R (Postive/Negative)
# 675 participants enrolled in each site
set.seed(3455)
sim_serodata <- data.frame(SITEID = c(rep("Punjab", 675), rep("Assam", 675)),
                           AGE = runif(675*2, 0, 100), 
                           REVQUAL_M = round(runif(675*2, 0, 1)), 
                           REVQUAL_R = round(runif(675*2, 0, 1)))
sim_serodata %>%
  mutate(REVQUAL_M = case_when(REVQUAL_M == 0 ~ "Negative", 
                               REVQUAL_M == 1 ~ "Positive"), 
         REVQUAL_R = case_when(REVQUAL_R == 0 ~ "Negative", 
                               REVQUAL_R == 1 ~ "Positive")) -> sim_serodata

# Assigning serology data to district locations
# rmultinom (number of draws, size = total # of individuals to distribute,
# prob = vector of human pop or other factor to distribute by)
punjab$samples <- as.vector(rmultinom(n = 1, size = nrow(sim_serodata %>% filter(SITEID == "Punjab")),
                                      prob = punjab$india_pop))
assam$samples <- as.vector(rmultinom(n = 1, size = nrow(sim_serodata %>% filter(SITEID == "Assam")),
                                     prob = assam$india_pop))
alloc_punjab <- rep(punjab$name_2, punjab$samples)
alloc_assam <- rep(assam$name_2, assam$samples)

# The jumble function that participants will have learned from earlier R exercises
jumble <- function(x) {
  jumbled <- x[order(runif(length(x), min = 0, max = 1))]
  return(jumbled)
}

set.seed(45601)
sim_serodata %>%
  arrange(SITEID) %>% # in ascending order with Assam 1st!
  mutate(district = c(as.character(jumble(alloc_assam)),
                      as.character(jumble(alloc_punjab)))) -> sim_serodata
write_csv(sim_serodata, "data/serodata_spatial.csv")
