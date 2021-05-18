
### Code: SDM using Maxent with future projections for the Cao Vit Gibbon

###----------------------------------------------------------------------------------------------------

# Set up working directory
workdir <- "E:/D/Articles/2020/Paper_CaoVit/SDM/"

###----------------------------------------------------------------------------------------------------

###  1. Read in occurrences and spatially thin to reduce sampling biases
# Load occurrences
Locs <- read.csv(paste0(workdir, 'Occ/sp.csv'))

# Using spThin, thin all occurrences using a 5km distance, save and load back in
library(spThin)
tLocs <- thin(loc.data = Locs, lat.col = 'Lat', long.col = 'Long', spec.col = 'Species', thin.par = 5, reps = 10, write.files = F, 
              max.files = 1, locs.thinned.list.return = T, write.log.file = F)
write.csv(tLocs[10], paste0(workdir, "Occ/UsableLocs.csv"))
Locs <- read.csv(paste0(workdir, "Occ/UsableLocs.csv"))

###----------------------------------------------------------------------------------------------------

### 2. Setting up a background extent
# Read Global climate data, and crop to managable size 
library(dismo)
library(raster)

globalbio<-stack(list.files(path = 'E:/D/GIS Data/1_BioCurrent', pattern = '\\.tif$',full.names = T))

# Create general extent Exact values must be chosen by species experts
e<-extent(c(95, 115, 15, 35))

# Crop by this extent
vietnam<-crop(globalbio, e)

# Remove the global climate data to save space
rm(globalbio)

# Create a minimum complex polygon buffered by 2.5 degrees. This distance must be selected by species experts. 
library(rgeos)
mcp <- function (xy) {
  xy <- as.data.frame(coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1))))
}
buff.dist <- 2.5
MCP.locs<-mcp(Locs[,2:3])
shp <- gBuffer(MCP.locs, width = buff.dist)
climateOnly <- mask(vietnam, shp)
backg <- randomPoints(climateOnly, 10000)
write.csv(backg, paste0(workdir, "Occ/background.csv"))
backg<-read.csv(paste0(workdir, "Occ/background.csv"))[,c(2,3)]
colnames(backg) <- c("Long", "Lat")

###----------------------------------------------------------------------------------------------------

### 3. Modeling with climate only modeling: Modeling using the Maxent algorithm. 

# Give R enough memory
options(java.parameters = "-Xmx20000m")

library(ENMeval)
Climate.only.res <- ENMevaluate(occ = Locs[,2:3], env = climateOnly, RMvalues = seq(1,10,0.5), fc = c('L','LQ','H','LQH'), 
                                method = 'jackknife', rasterPreds = T, bg.coords = backg, parallel = T, numCores = 4, 
                                algorithm = "maxent.jar", clamp = F)

# a. Selecting optimal models

# Define optimize function
#  optimize function
optimize <- function(res) {
  ###Remove any candidate model which has an AUC less than 0.51= models with no discrimination
  opt.auc <- res[res$train.AUC >= 0.5,]
  ###Remove any candidates for which AIC was not calculated
  no.param <- opt.auc[opt.auc$parameters > 1,]
  #no.param <- opt.auc[!is.na(opt.auc$AICc),]
  ###Remove any candidates where the AIC score was NA (too many parameters) 
  noAICNA <- no.param[!is.na(no.param$parameters),]
  #noAICNA<- no.param[which(!is.na(no.param$AICc)),]
  ###Remove any models which have an OR of zero
  noOR0 <- noAICNA[noAICNA$avg.test.or10pct != 0,]
  ###Order the remaining list by lowest OR then highest AUC, sequentially
  ordered<-noOR0[with(noOR0, order(avg.test.or10pct, -avg.test.AUC)), ]
  ###Grab the settings of that first model (the optimal model)
  ordered[1,]
}
optimize(Climate.only.res@results)


# Project the optimal model to the larger area. Set the arguments to match the settings from the *optimize* function.
ClimateOnly.mod <- maxent(
  x = climateOnly, # bio stack
  p = Locs[,2:3], # locality csv
  a = backg, # background coords
  path= "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/ModelTest", # path to save to
  args=c(
    'betamultiplier=3.5',
    'linear=false', 
    'quadratic=false',
    'hinge=true',
    'product=false',
    'threshold=false',
    'threads=2',
    'responsecurves=true',
    'jackknife=true',
    'askoverwrite=false',
    'autofeature=false'
  )
)

climateOnly.Pred<- predict(
  object = ClimateOnly.mod,
  x = vietnam,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/ModelTest/Predict", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.Pred)

### Raster classification for current potential distribution

#  Access the cloglog 10% training presence threshold
TenTP <- subset(ClimateOnly.mod@results, rownames(ClimateOnly.mod@results) %in% "X10.percentile.training.presence.Cloglog.threshold")[[1]]

# Threshold each model at that value, setting values below to 0, and above to 1
climateOnly.Pred[climateOnly.Pred >= TenTP] <- 1
climateOnly.Pred[climateOnly.Pred < TenTP] <- 0

writeRaster(climateOnly.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/ModelTest/Reclassed_Predict", format = 'GTiff', overwrite = F, args = "cloglog")

###----------------------------------------------------------------------------------------------------

### 4. Future projections. Climate models and scenarios must be considered by experts. 

# BCC-CSM2-MR 2041-2060
BC.26.41<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/BCC-CSM2-MR_ssp126_2041-2060.tif")
BC.45.41<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/BCC-CSM2-MR_ssp245_2041-2060.tif")
BC.70.41<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/BCC-CSM2-MR_ssp370_2041-2060.tif")
BC.85.41<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/BCC-CSM2-MR_ssp585_2041-2060.tif")

# BCC-CSM2-MR 2061-2080
BC.26.61<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/BCC-CSM2-MR_ssp126_2061-2080.tif")
BC.45.61<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/BCC-CSM2-MR_ssp245_2061-2080.tif")
BC.70.61<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/BCC-CSM2-MR_ssp370_2061-2080.tif")
BC.85.61<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/BCC-CSM2-MR_ssp585_2061-2080.tif")

# MIROC6 2041-2060
MC.26.41<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/MIROC6_ssp126_2041-2060.tif")
MC.45.41<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/MIROC6_ssp245_2041-2060.tif")
MC.70.41<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/MIROC6_ssp370_2041-2060.tif")
MC.85.41<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/MIROC6_ssp585_2041-2060.tif")

# MIROC6 2061-2080
MC.26.61<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/MIROC6_ssp126_2061-2080.tif")
MC.45.61<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/MIROC6_ssp245_2061-2080.tif")
MC.70.61<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/MIROC6_ssp370_2061-2080.tif")
MC.85.61<-stack("E:/D/Work/Project/2019 562/Map/RasterFuture/MIROC6_ssp585_2061-2080.tif")

### Crop all future scenarios to smaller extent (Important for computer performance)
BC.26.41<-crop(BC.26.41, e)
BC.45.41<-crop(BC.45.41, e)
BC.70.41<-crop(BC.70.41, e)
BC.85.41<-crop(BC.85.41, e)

BC.26.61<-crop(BC.26.61, e)
BC.45.61<-crop(BC.45.61, e)
BC.70.61<-crop(BC.70.61, e)
BC.85.61<-crop(BC.85.61, e)

MC.26.41<-crop(MC.26.41, e)
MC.45.41<-crop(MC.45.41, e)
MC.70.41<-crop(MC.70.41, e)
MC.85.41<-crop(MC.85.41, e)

MC.26.61<-crop(MC.26.61, e)
MC.45.61<-crop(MC.45.61, e)
MC.70.61<-crop(MC.70.61, e)
MC.85.61<-crop(MC.85.61, e)


# BCC-CSM2-MR SSP1-2.6 in 2041-2060
names(BC.26.41) <- names(vietnam)

climateOnly.BC.26.41.Pred<- predict(
  object = ClimateOnly.mod,
  x = BC.26.41,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/bc26bi41", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.BC.26.41.Pred)

climateOnly.BC.26.41.Pred[climateOnly.BC.26.41.Pred >= TenTP] <- 1
climateOnly.BC.26.41.Pred[climateOnly.BC.26.41.Pred < TenTP] <- 0

writeRaster(climateOnly.BC.26.41.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_bc26bi41", format = 'GTiff', overwrite = F, args = "cloglog")

# BCC-CSM2-MR SSP2-4.5 in 2041-2060
names(BC.45.41) <- names(vietnam)

climateOnly.BC.45.41.Pred<- predict(
  object = ClimateOnly.mod,
  x = BC.45.41,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/bc45bi41", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.BC.45.41.Pred)

climateOnly.BC.45.41.Pred[climateOnly.BC.45.41.Pred >= TenTP] <- 1
climateOnly.BC.45.41.Pred[climateOnly.BC.45.41.Pred < TenTP] <- 0

writeRaster(climateOnly.BC.45.41.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_bc45bi41", format = 'GTiff', overwrite = F, args = "cloglog")

# BCC-CSM2-MR SSP3-7.0 in 2041-2060
names(BC.70.41) <- names(vietnam)

climateOnly.BC.70.41.Pred<- predict(
  object = ClimateOnly.mod,
  x = BC.70.41,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/bc70bi41", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.BC.70.41.Pred)

climateOnly.BC.70.41.Pred[climateOnly.BC.70.41.Pred >= TenTP] <- 1
climateOnly.BC.70.41.Pred[climateOnly.BC.70.41.Pred < TenTP] <- 0

writeRaster(climateOnly.BC.70.41.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_bc70bi41", format = 'GTiff', overwrite = F, args = "cloglog")

# BCC-CSM2-MR SSP5-8.5 in 2041-2060
names(BC.85.41) <- names(vietnam)

climateOnly.BC.85.41.Pred<- predict(
  object = ClimateOnly.mod,
  x = BC.85.41,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/bc85bi41", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.BC.85.41.Pred)

climateOnly.BC.85.41.Pred[climateOnly.BC.85.41.Pred >= TenTP] <- 1
climateOnly.BC.85.41.Pred[climateOnly.BC.85.41.Pred < TenTP] <- 0

writeRaster(climateOnly.BC.85.41.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_bc85bi41", format = 'GTiff', overwrite = F, args = "cloglog")

# BCC-CSM2-MR SSP1-2.6 in 2061-2080
names(BC.26.61) <- names(vietnam)

climateOnly.BC.26.61.Pred<- predict(
  object = ClimateOnly.mod,
  x = BC.26.61,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/bc26bi61", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.BC.26.61.Pred)

climateOnly.BC.26.61.Pred[climateOnly.BC.26.61.Pred >= TenTP] <- 1
climateOnly.BC.26.61.Pred[climateOnly.BC.26.61.Pred < TenTP] <- 0

writeRaster(climateOnly.BC.26.61.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_bc26bi61", format = 'GTiff', overwrite = F, args = "cloglog")

# BCC-CSM2-MR SSP2-4.5 in 2061-2080
names(BC.45.61) <- names(vietnam)

climateOnly.BC.45.61.Pred<- predict(
  object = ClimateOnly.mod,
  x = BC.45.61,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/bc45bi61", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.BC.45.61.Pred)

climateOnly.BC.45.61.Pred[climateOnly.BC.45.61.Pred >= TenTP] <- 1
climateOnly.BC.45.61.Pred[climateOnly.BC.45.61.Pred < TenTP] <- 0

writeRaster(climateOnly.BC.45.61.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_bc45bi61", format = 'GTiff', overwrite = F, args = "cloglog")

# BCC-CSM2-MR SSP3-7.0 in 2061-2080
names(BC.70.61) <- names(vietnam)

climateOnly.BC.70.61.Pred<- predict(
  object = ClimateOnly.mod,
  x = BC.70.61,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/bc70bi61", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.BC.70.61.Pred)

climateOnly.BC.70.61.Pred[climateOnly.BC.70.61.Pred >= TenTP] <- 1
climateOnly.BC.70.61.Pred[climateOnly.BC.70.61.Pred < TenTP] <- 0

writeRaster(climateOnly.BC.70.61.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_bc70bi61", format = 'GTiff', overwrite = F, args = "cloglog")

# BCC-CSM2-MR SSP5-8.5 in 2061-2080
names(BC.85.61) <- names(vietnam)

climateOnly.BC.85.61.Pred<- predict(
  object = ClimateOnly.mod,
  x = BC.85.61,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/bc85bi61", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.BC.85.61.Pred)

climateOnly.BC.85.61.Pred[climateOnly.BC.85.61.Pred >= TenTP] <- 1
climateOnly.BC.85.61.Pred[climateOnly.BC.85.61.Pred < TenTP] <- 0

writeRaster(climateOnly.BC.85.61.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_bc85bi61", format = 'GTiff', overwrite = F, args = "cloglog")

# MIROC6 SSP1-2.6 in 2041-2060
names(MC.26.41) <- names(vietnam)

climateOnly.MC.26.41.Pred<- predict(
  object = ClimateOnly.mod,
  x = MC.26.41,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/mc26bi41", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.MC.26.41.Pred)

climateOnly.MC.26.41.Pred[climateOnly.MC.26.41.Pred >= TenTP] <- 1
climateOnly.MC.26.41.Pred[climateOnly.MC.26.41.Pred < TenTP] <- 0

writeRaster(climateOnly.MC.26.41.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_mc26bi41", format = 'GTiff', overwrite = F, args = "cloglog")

# MIROC6 SSP2-4.5 in 2041-2060
names(MC.45.41) <- names(vietnam)

climateOnly.MC.45.41.Pred<- predict(
  object = ClimateOnly.mod,
  x = MC.45.41,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/mc45bi41", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.MC.45.41.Pred)

climateOnly.MC.45.41.Pred[climateOnly.MC.45.41.Pred >= TenTP] <- 1
climateOnly.MC.45.41.Pred[climateOnly.MC.45.41.Pred < TenTP] <- 0

writeRaster(climateOnly.MC.45.41.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_mc45bi41", format = 'GTiff', overwrite = F, args = "cloglog")

# MIROC6 SSP3-7.0 in 2041-2060
names(MC.70.41) <- names(vietnam)

climateOnly.MC.70.41.Pred<- predict(
  object = ClimateOnly.mod,
  x = MC.70.41,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/mc70bi41", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.MC.70.41.Pred)

climateOnly.MC.70.41.Pred[climateOnly.MC.70.41.Pred >= TenTP] <- 1
climateOnly.MC.70.41.Pred[climateOnly.MC.70.41.Pred < TenTP] <- 0

writeRaster(climateOnly.MC.70.41.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_mc70bi41", format = 'GTiff', overwrite = F, args = "cloglog")

# MIROC6 SSP5-8.5 in 2041-2060
names(MC.85.41) <- names(vietnam)

climateOnly.MC.85.41.Pred<- predict(
  object = ClimateOnly.mod,
  x = MC.85.41,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/mc85bi41", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.MC.85.41.Pred)

climateOnly.MC.85.41.Pred[climateOnly.MC.85.41.Pred >= TenTP] <- 1
climateOnly.MC.85.41.Pred[climateOnly.MC.85.41.Pred < TenTP] <- 0

writeRaster(climateOnly.MC.85.41.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_mc85bi41", format = 'GTiff', overwrite = F, args = "cloglog")

# MIROC6 SSP1-2.6 in 2061-2080
names(MC.26.61) <- names(vietnam)

climateOnly.MC.26.61.Pred<- predict(
  object = ClimateOnly.mod,
  x = MC.26.61,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/mc26bi61", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.MC.26.61.Pred)

climateOnly.MC.26.61.Pred[climateOnly.MC.26.61.Pred >= TenTP] <- 1
climateOnly.MC.26.61.Pred[climateOnly.MC.26.61.Pred < TenTP] <- 0

writeRaster(climateOnly.MC.26.61.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_mc26bi61", format = 'GTiff', overwrite = F, args = "cloglog")

# MIROC6 SSP2-4.5 in 2061-2080
names(MC.45.61) <- names(vietnam)

climateOnly.MC.45.61.Pred<- predict(
  object = ClimateOnly.mod,
  x = MC.45.61,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/mc45bi61", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.MC.45.61.Pred)

climateOnly.MC.45.61.Pred[climateOnly.MC.45.61.Pred >= TenTP] <- 1
climateOnly.MC.45.61.Pred[climateOnly.MC.45.61.Pred < TenTP] <- 0

writeRaster(climateOnly.MC.45.61.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_mc45bi61", format = 'GTiff', overwrite = F, args = "cloglog")

# MIROC6 SSP3-7.0 in 2061-2080
names(MC.70.61) <- names(vietnam)

climateOnly.MC.70.61.Pred<- predict(
  object = ClimateOnly.mod,
  x = MC.70.61,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/mc70bi61", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.MC.70.61.Pred)

climateOnly.MC.70.61.Pred[climateOnly.MC.70.61.Pred >= TenTP] <- 1
climateOnly.MC.70.61.Pred[climateOnly.MC.70.61.Pred < TenTP] <- 0

writeRaster(climateOnly.MC.70.61.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_mc70bi61", format = 'GTiff', overwrite = F, args = "cloglog")

# MIROC6 SSP5-8.5 in 2061-2080
names(MC.85.61) <- names(vietnam)

climateOnly.MC.85.61.Pred<- predict(
  object = ClimateOnly.mod,
  x = MC.85.61,
  filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/mc85bi61", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)
plot(climateOnly.MC.85.61.Pred)

climateOnly.MC.85.61.Pred[climateOnly.MC.85.61.Pred >= TenTP] <- 1
climateOnly.MC.85.61.Pred[climateOnly.MC.85.61.Pred < TenTP] <- 0

writeRaster(climateOnly.MC.85.61.Pred, filename = "E:/D/Articles/2020/Paper_CaoVit/SDM/Result/Future/Reclassed_mc85bi61", format = 'GTiff', overwrite = F, args = "cloglog")

