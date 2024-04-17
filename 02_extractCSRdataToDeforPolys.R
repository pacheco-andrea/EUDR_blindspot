# EUDR-Soy Moratorium project

# This script extracts the sum of the means of the legal/ilegal rasters per deforestation polygon
# i write them out as individual shapefiles, per year, to handle the computation by parts
# author: Andrea Pacheco
# first run: 18.03.2024


#libraries
# libraries
library(terra)
library(sf)
library(dplyr)
library(exactextractr)

#directory
wdmain <- "N:/eslu/work/CLEVER/EUDR_soyMoratorium/"
setwd(wdmain)

# get the deforestation polygons
deforPoly <- st_read("data/processed/DeforestationPolygons_INPE/validGEOMETRIESBETWEEN2008E2020_deforestation_polygonsREPROJECTED.shp")
deforPoly
unique(deforPoly$year)
length(unique(deforPoly$fid))
deforPoly <- select(deforPoly, c("fid", "year"))

deforPoly <- base::split(deforPoly, as.factor(deforPoly$year))
length(deforPoly)
names(deforPoly)

# get the illegal-legal rasters ----
setwd(paste0(wdmain,"data/processed/ForestCode_deficit-surplus_CSR"))

# conduct extractions ----
# make several loops (bc i'm lazy), for extracting the values, writing out the values per year
# NOTE: these values are all in (ha)!!!!

# deficit of native vegetation for rl ----
r <- rast("rl_def_1km.tif")
r

for(i in 1:length(deforPoly))
{
  # for each of the deforestation polygons (within each i year), extract how much was in deficit
  value <- exactextractr::exact_extract(r, deforPoly[[i]], fun = "sum")
  table <- cbind(deforPoly[[i]], value)
  # write out the extraction for that year
  setwd(paste0(wdmain, "data/processed/DeforPolys_DeficitSurplusExtractions"))
  st_write(table, paste0("rl_def", "_", names(deforPoly)[i], ".shp"), append = F)
}

# deficit of native vegetation for app ----
setwd(paste0(wdmain,"data/processed/ForestCode_deficit-surplus_CSR"))
r <- rast("app_def_1km.tif")
# plot(r, breaks = c(0, -1000, -2500, -5000))

for(i in 1:length(deforPoly))
{
  # for each of the deforestation polygons (within each i year), extract how much was in deficit
  value <- exactextractr::exact_extract(r, deforPoly[[i]], fun = "sum")
  table <- cbind(deforPoly[[i]], value)
  # write out the extraction for that year
  setwd(paste0(wdmain, "data/processed/DeforPolys_DeficitSurplusExtractions"))
  st_write(table, paste0("app_def", "_", names(deforPoly)[i], ".shp"), append = F)
}

# surplus of native vegetation ----
setwd(paste0(wdmain,"data/processed/ForestCode_deficit-surplus_CSR"))
r <- rast("rl_ativo_1km.tif") 
# plot(r)

for(i in 1:length(deforPoly))
{
  # for each of the deforestation polygons (within each i year), extract how much was in deficit
  value <- exactextractr::exact_extract(r, deforPoly[[i]], fun = "sum")
  table <- cbind(deforPoly[[i]], value)
  # write out the extraction for that year
  setwd(paste0(wdmain, "data/processed/DeforPolys_DeficitSurplusExtractions"))
  st_write(table, paste0("rl_ativo", "_", names(deforPoly)[i], ".shp"), append = F)
}

# deforestation post-2008 ----
setwd(paste0(wdmain,"data/processed/ForestCode_deficit-surplus_CSR"))
r <- rast("desmat_p08_1km.tif") 
# plot(r)

for(i in 1:length(deforPoly))
{
  # for each of the deforestation polygons (within each i year), extract how much was in deficit
  value <- exactextractr::exact_extract(r, deforPoly[[i]], fun = "sum")
  table <- cbind(deforPoly[[i]], value)
  # write out the extraction for that year
  setwd(paste0(wdmain, "data/processed/DeforPolys_DeficitSurplusExtractions"))
  st_write(table, paste0("desmat_p08", "_", names(deforPoly)[i], ".shp"), append = F)
}

