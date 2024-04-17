# EUDR-Soy Moratorium project

# This script spatially intersects:
# the deforestation polygons from INPE
# the rural properties from CSR and their forest compliance (legal and illegal deforestation)
# output: one.shp per year of the overlapping parts of the two datasets above
# author: Andrea Pacheco


#libraries
# libraries
library(terra)
library(sf)
library(dplyr)

#directory
wdmain <- "N:/eslu/work/CLEVER/EUDR_soyMoratorium/"
my_crs_SAaea <- "+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"


# get the deforestation polygons ----
setwd(wdmain)
deforPoly <- st_read("data/processed/DeforestationPolygons_INPE/validGEOMETRIESBETWEEN2008E2020_deforestation_polygonsREPROJECTED.shp")
deforPoly <- select(deforPoly, c("fid", "year"))
length(unique(deforPoly$fid))
deforPoly <- st_transform(deforPoly, my_crs_SAaea)

# visualize this deforestation per year
data <- deforPoly
data$area <- as.numeric((st_area(data))/10000) 

ggplot(data) +
  geom_col(aes(year, (area/100))) +
  # scale_fill_manual(values = mycols) +
  labs(y = bquote("Area km"^2), fill = "Deforestation") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(breaks = c(2008:2020)) +
  theme(panel.background = element_rect(fill = "transparent"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.position = c(0.77, 0.87))


# handle each year as a separate list
deforPoly <- base::split(deforPoly, as.factor(deforPoly$year))
length(deforPoly)

# get CAR data ----
setwd(paste0(wdmain,"/data/raw/LandTenure_v20231009/"))
l <- list.files()
files <- l[grep(".shp", l)]

# loop to handle intersection for: 
StateIntersections <- list()

# for each of the years
for(j in 1:length(deforPoly))
{
  
  # for each of the states
  for (i in 1:length(files))
  {
    setwd(paste0(wdmain,"/data/raw/LandTenure_v20231009/"))
    # read in shapefile
    stateShp <- st_read(files[i])
    name <- paste(files[i])
    head(stateShp)
    # select columns we care about, in particular:
    # 'rl_ativo', 'Vegetação nativa acima dos mínimo requerido para reserva legal (hectares)'],
    # 'rl_def', 'Déficit de cobertura vegetal para o cumprimento da área mínima exigida para reserva legal (ha)'],
    # 'app_def', 'Déficit de vegetação para o cumprimento da exigência de Área de Preservação Permanente (APP) ripária para fins de restauro (ha)'],
    # 'desmat_p08', 'Desmatamento pós 2008 (ha) do PRODES']
    stateShp <- select(stateShp, c("uf", "tipo","rl_ativo", "rl_def","app_def","desmat_p08"))
    stateShp <- st_transform(stateShp, my_crs_SAaea)
    
    # fix bug in 2019 where there's an invalid geometry
    if(deforPoly[[j]]$year[1] == 2019){
      deforPoly[[j]] <- st_make_valid(deforPoly[[j]])
    }
    
    # intersect one year with one state at a time
    StateIntersections[[i]] <- st_intersection(deforPoly[[j]], stateShp)
    # StateIntersections[[i]]
    print(i)
  }
  # bind the rows of those intersections
  yIntersection <- do.call(rbind, StateIntersections)
  # make sure we only keep the polygons (and not linestring geometries)
  yIntersection <- st_collection_extract(yIntersection, "POLYGON")
  setwd(paste0(wdmain,"/data/processed/PolyIntersections/"))
  st_write(yIntersection, paste0("CSR-Deforest_intersection_", names(deforPoly)[j] ,".shp"), append = FALSE)
  print(j)
}

