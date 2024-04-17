# EUDR-Soy Moratorium project

# This script rasterizes the legal and illegal deforestation within the CAR properties from the CSR
# author: Andrea Pacheco
# first run: 18.03.2024


#libraries
# libraries
library(terra)
library(sf)
library(dplyr)

#directory
wdmain <- "N:/eslu/work/CLEVER/EUDR_soyMoratorium/"

# 0. make mask to base rest of rasters on
setwd("N:/eslu/priv/pacheco/whoOwnsBRBD//data/raw/Biodiversity_v20231009") # use my biodiversity data
r <- rast(list.files()[grep("Richness", list.files())[1]])
my_crs_SAaea <- "+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
r <- project(r, my_crs_SAaea)
mask <- r*0


# get CAR data ----
setwd(paste0(wdmain,"/data/raw/LandTenure_v20231009/"))

l <- list.files()
files <- l[grep(".shp", l)]

# create loop for rasterizing each of these files
# categorizing if there is a surplus or deficit of the Forest code
# loop through reading and writing simplified shapefiles
s <- list()
for (i in 1:length(files))
{
  setwd(paste0(wdmain,"/data/raw/LandTenure_v20231009/"))
  # read in shapefiles
  stateShp <- st_read(files[i])
  name <- paste(files[i])
  head(stateShp)
  # select columns we care about, in particular:
  # 'rl_ativo', 'Vegetação nativa acima dos mínimo requerido para reserva legal (hectares)'],
  # 'rl_def', 'Déficit de cobertura vegetal para o cumprimento da área mínima exigida para reserva legal (ha)'],
  # 'app_def', 'Déficit de vegetação para o cumprimento da exigência de Área de Preservação Permanente (APP) ripária para fins de restauro (ha)'],
  # 'desmat_p08', 'Desmatamento pós 2008 (ha) do PRODES']
  s[[i]] <- select(stateShp, c("uf", "tipo","rl_ativo", "rl_def","app_def","desmat_p08"))
  s[[i]] <- st_transform(s[[i]], my_crs_SAaea)
}
s2 <- do.call(rbind,s)

# rasterize and write out ----

# run smaller tests before
test <- s2[1:1000,]
summary(test)
lapply(test, class)

test_any <- terra::rasterize(test, mask, field = "rl_ativo", fun = "count", background = NA)
test_sum <- terra::rasterize(test, mask, field = "rl_ativo", fun = "sum", background = NA)
test_max <- terra::rasterize(test, mask, field = "rl_ativo", fun = "max", background = NA) #note this would be min for negative values
# test by binarizing
test$rl_ativo2 <- 0
test[which(test$rl_ativo > 0),]$rl_ativo2 <- 1
# this would count the number of polygons that have active native veg
test_again <- terra::rasterize(test, mask, field = "rl_ativo2", fun = "count", background=NA)
par(mfrow = c(2,2))
plot(test_any)
plot(test_sum)
plot(test_max)
plot(test_again)
dev.off()

# test for the negative vals as well
test_any <- terra::rasterize(test, mask, field = "rl_def", background = NA)
test_count <- terra::rasterize(test, mask, field = "rl_def", fun = "count", background = NA)
test_sum <- terra::rasterize(test, mask, field = "rl_def", fun = "sum", background = NA)
test_max <- terra::rasterize(test, mask, field = "rl_def", fun = "max", background = NA) #note this would be min for negative values
test$rl_def2 <- 0
test[which(test$rl_def < 0),]$rl_def2 <- 1
test_again <- terra::rasterize(test, mask, field = "rl_def2", fun = "count", background=NA)
par(mfrow = c(2,3))
plot(test_any)
plot(test_again) # the same...
plot(test_count)
plot(test_sum)
plot(test_max)
dev.off()


# rasterize for the whole thing: ----
setwd(paste0(wdmain,"data/processed/ForestCode_deficit-surplus_CSR"))

r <- terra::rasterize(s2, mask, "rl_ativo", fun = "count", background = NA)
writeRaster(r, filename = "rl_ativo_1km.tif", overwrite = TRUE)

r <- terra::rasterize(s2, mask, "rl_def", fun = "count", background = NA)
writeRaster(r, filename = "rl_def_1km.tif", overwrite = TRUE)

r <- terra::rasterize(s2, mask, "app_def",fun = "count", background = NA)
writeRaster(r, filename = "app_def_1km.tif", overwrite = TRUE)

r <- terra::rasterize(s2, mask, "desmat_p08", fun = "count", background = NA)
writeRaster(r, filename = "desmat_p08_1km.tif", overwrite = TRUE)


# st_write(s2, "CSR_FCdeficit-surplus.shp", append = FALSE)
