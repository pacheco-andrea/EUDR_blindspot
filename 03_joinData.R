# EUDR-Soy Moratorium project

# this script puts all the data together for making the bar graph
# and finds out how much deforestation was legal/illegal
# author: Andrea Pacheco


# libraries
library(sf)
library(dplyr)
library(ggplot2)

#directory
wdmain <- "N:/eslu/work/CLEVER/EUDR_soyMoratorium/"
setwd(wdmain)

# get the polygon intersections ----
# i.e., the properties that overlapped with deforestation polygons
setwd(paste0(wdmain, "data/processed/PolyIntersections"))
l <- list.files()
s <- grep(".shp",l)
data <- lapply(l[s], st_read)
data[[1]]
# check that these variables are actually different across one year
lapply(data, summary)

# next, put all data together
unlist(lapply(data, nrow))
data <- do.call(rbind, data)
data$area <- as.numeric((st_area(data))/10000) # convert from m^2 to ha to match the other vars
summary(data)
data$year <- as.factor(data$year)


# NOTE: there are duplicated fids in this data
# this is because although the deforestation polygons were unique obs, 
# they can overlap with multiple properties. the resulting polygons are all those areas which overlapped. 

# 
length(unique(data$fid)) # the original number is 552752
# this is 69,262 observations less than the INPE deforestation - but 
test <- data[which(duplicated(data$fid)),]
test[which(test$fid == 239903 ),]

plot(deforPoly[which(deforPoly$fid == 239903 ),]$geometry)
plot(test[which(test$fid == 239903 ),]$geometry, add = T)
plot(test[which(test$fid == 239903 ),"tipo"])

# from this, I can graph: ----

# how much deforestation happened illegally (in properties with deficit)?
# how much deforestation happened legally (in properties with surplus)?

# create legality variable
# we need to set this as a starting point because there are deforestation polygons that have neither surplus nor deficit - in theory, legal
data$legality <- "legal" 
# identify cases where there is veg surplus
# would also be legal, so the part below is redundant
# data[which(data$rl_ativo > 0),]$legality <- "legal"

# now identify the observations with deforestation where there was a deficit for either RL or APPs
# note this could overwrite the places where there was rl ativo (surplus), because app deficit is still be potentially illegal
nrow(data[which(data$rl_def < 0 | data$app_def < 0),])
data[which(data$rl_def < 0 | data$app_def < 0),]$legality <- "illegal"
data$legality <- as.factor(data$legality)
summary(data)

# get summarized figures
data %>%
  st_drop_geometry() %>%
  group_by(legality) %>%
  summarize(sum = sum(area))

mycols <- c("#cc4c02", "#fee391")

# plot legal and illegal deforestation
deforestation <- ggplot(data) +
  geom_col(aes(year, (area/100), fill = legality)) +
  scale_fill_manual(values = mycols) +
  labs(y = bquote("Area km"^2), fill = "Deforestation") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(panel.background = element_rect(fill = "transparent"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.position = c(0.77, 0.87))
deforestation

# this is freaking me out so i should just compare with the original INPE data

setwd(paste0(wdmain, "output/"))
png("illegalDeforestationPlot_2.png", units = "cm", width = 16, height = 6, res = 300)
deforestation
dev.off()
# write out data too
setwd(paste0(wdmain, "output/illegalDeforestationPlot_Data"))
st_write(data, "illegalDeforestatonData_v20240417.shp", append = F)

# explore the deforestation across tenure categories in these data?
ggplot(data) +
  geom_col(aes(year, (area/100), fill = tipo)) +
  # scale_fill_manual(values = mycols) +
  labs(y = bquote("Area km"^2), fill = "Deforestation") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(panel.background = element_rect(fill = "transparent"),
        axis.title.x = element_blank())


