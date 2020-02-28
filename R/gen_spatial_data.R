### Generate outputs for mapping tutorial
## Mal Rajeev
## Nov 2018
library(raster)
library(tidyverse)
library(magrittr)
library(rgdal)

## Aggregate india world pop so that it's more manageable
# india_pop <- raster("~/Google Drive/IND_ppp_2015_adj_v2/IND_ppp_2015_adj_v2.tif")
# india_pop <- aggregate(india_pop, fact = 10, fun = sum, na.rm = TRUE)
# writeRaster(india_pop, "data/india_pop.tif", overwrite = TRUE)
# india_births <- raster("data/india_births.tif")
# india_births <- aggregate(india_births, fact = 10, fun = sum, na.rm = TRUE)
# writeRaster(india_births, "data/india_births.tif", overwrite = TRUE)

## Get India admin 2 shapefile
# library(malariaAtlas)
# india_shp <- malariaAtlas::getShp(country = "India", admin_level = "admin2")
# writeOGR(india_shp, "data/India", "India", driver = "ESRI Shapefile")

## Read in shapefile and pop file
india_shp <- readOGR("data/India/India.shp")
india_pop <- raster("data/india_pop.tif")

## Getting punjab and assam
punjab <- subset(india_shp, name_1 == "Punjab")
assam <- subset(india_shp, name_1 == "Assam")

assam_pop <- crop(india_pop, assam) ## First we crop our india_pop raster to Assam
assam_pop <- mask(assam_pop, assam) ## Then we have to mask this so that all values outside of Assam are set to NA

punjab_pop <- crop(india_pop, punjab) 
punjab_pop <- mask(punjab_pop, punjab) 

## Extract pops
assam <- extract(assam_pop, assam, fun = sum, na.rm = TRUE, sp = TRUE) 
punjab <- extract(punjab_pop, punjab, fun = sum, na.rm = TRUE, sp = TRUE)

## Assigning serology data to individuals weighted by pop size
# rmultinom (number of draws, size = total # of individuals to distribute,
# prob = vector of human pop or other factor to distribute by)
load("data/data.RData")
head(df.eia)

punjab$samples <- as.vector(rmultinom(n = 1, size = nrow(df.eia %>% filter(SITEID == "Punjab")),
                                      prob = punjab$india_pop))
assam$samples <- as.vector(rmultinom(n = 1, size = nrow(df.eia %>% filter(SITEID == "Assam")),
                                     prob = assam$india_pop))
alloc_punjab <- rep(punjab$name_2, punjab$samples)
alloc_assam <- rep(assam$name_2, assam$samples)
jumble <- function(x) {
  jumbled <- x[order(runif(length(x), min = 0, max = 1))]
  return(jumbled)
}

df.eia %>%
  arrange(SITEID) %>% ## in ascending order with Assam 1st!
  mutate(district = c(as.character(jumble(alloc_assam)),
                      as.character(jumble(alloc_punjab)))) -> df.eia
df.eia
write_csv(df.eia, "data/serodata_spatial.csv")
