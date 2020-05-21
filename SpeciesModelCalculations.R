library(raster)
source("SI_Functions.R")
##########  Load SDM projections ############
##### Francois Langur
FLCurrentSDM <- raster("")
## 2040-2061
FLBC.SSP2.4.5.2041.2060 <- raster("")
FLBC.SSP3.7.0.2041.2060 <- raster("")
FLBC.SSP5.8.5.2041.2060 <- raster("")
FLMG.SSP2.4.5.2041.2060 <- raster("")
FLMG.SSP3.7.0.2041.2060 <- raster("")
FLMG.SSP5.8.5.2041.2060 <- raster("")
FLMC.SSP2.4.5.2041.2060 <- raster("")
FLMC.SSP3.7.0.2041.2060 <- raster("")
FLMC.SSP5.8.5.2041.2060 <- raster("")
FL## 2061-2080
FLBC.SSP2.4.5.2061.2080 <- raster("")
FLBC.SSP3.7.0.2061.2080 <- raster("")
FLBC.SSP5.8.5.2061.2080 <- raster("")
FLMG.SSP2.4.5.2061.2080 <- raster("")
FLMG.SSP3.7.0.2061.2080 <- raster("")
FLMG.SSP5.8.5.2061.2080 <- raster("")
FLMC.SSP2.4.5.2061.2080 <- raster("")
FLMC.SSP3.7.0.2061.2080 <- raster("")
FLMC.SSP5.8.5.2061.2080 <- raster("")
###### Range area change
threshold <- 
## 2050's
FLBC.SSP2.4.5.2041.2060RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection =FLBC.SSP2.4.5.2041.2060, threshold = threshold, projection = F)
FLBC.SSP3.7.0.2041.2060RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection =FLBC.SSP3.7.0.2041.2060, threshold = threshold, projection = F)
FLBC.SSP5.8.5.2041.2060RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection =FLBC.SSP5.8.5.2041.2060, threshold = threshold, projection = F)
FLMG.SSP2.4.5.2041.2060RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection =FLMG.SSP2.4.5.2041.2060, threshold = threshold, projection = F)
FLMG.SSP3.7.0.2041.2060RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection =FLMG.SSP3.7.0.2041.2060, threshold = threshold, projection = F)
FLMG.SSP5.8.5.2041.2060RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection =FLMG.SSP5.8.5.2041.2060, threshold = threshold, projection = F)
FLMC.SSP2.4.5.2041.2060RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection =FLMC.SSP2.4.5.2041.2060, threshold = threshold, projection = F)
FLMC.SSP3.7.0.2041.2060RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection =FLMC.SSP3.7.0.2041.2060, threshold = threshold, projection = F)
FLMC.SSP5.8.5.2041.2060RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection =FLMC.SSP5.8.5.2041.2060, threshold = threshold, projection = F)
## 2070's
FLBC.SSP2.4.5.2061.2080RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP2.4.5.2061.2080, threshold = threshold, projection = F)
FLBC.SSP3.7.0.2061.2080RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP3.7.0.2061.2080, threshold = threshold, projection = F)
FLBC.SSP5.8.5.2061.2080RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP5.8.5.2061.2080, threshold = threshold, projection = F)
FLMG.SSP2.4.5.2061.2080RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP2.4.5.2061.2080, threshold = threshold, projection = F)
FLMG.SSP3.7.0.2061.2080RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP3.7.0.2061.2080, threshold = threshold, projection = F)
FLMG.SSP5.8.5.2061.2080RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP5.8.5.2061.2080, threshold = threshold, projection = F)
FLMC.SSP2.4.5.2061.2080RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP2.4.5.2061.2080, threshold = threshold, projection = F)
FLMC.SSP3.7.0.2061.2080RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP3.7.0.2061.2080, threshold = threshold, projection = F)
FLMC.SSP5.8.5.2061.2080RAC <- rangeAreaChange(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP5.8.5.2061.2080, threshold = threshold, projection = F)

futures <- stack(list.files(path = , pattern = "\\.tif$", full.names=T))
futureRAC <- lapply(futures, rangeAreaChange, currentProjection=FLcurrentSDM, futureProjection = futures, threshold=threshold, projection=F)

###### Centroid Shift
threshold
## 2050's
FLBC.SSP2.4.5.2041.2060CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP2.4.5.2041.2060, threshold = threshold)
FLBC.SSP3.7.0.2041.2060CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP3.7.0.2041.2060, threshold = threshold)
FLBC.SSP5.8.5.2041.2060CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP5.8.5.2041.2060, threshold = threshold)
FLMG.SSP2.4.5.2041.2060CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP2.4.5.2041.2060, threshold = threshold)
FLMG.SSP3.7.0.2041.2060CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP3.7.0.2041.2060, threshold = threshold)
FLMG.SSP5.8.5.2041.2060CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP5.8.5.2041.2060, threshold = threshold)
FLMC.SSP2.4.5.2041.2060CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP2.4.5.2041.2060, threshold = threshold)
FLMC.SSP3.7.0.2041.2060CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP3.7.0.2041.2060, threshold = threshold)
FLMC.SSP5.8.5.2041.2060CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP5.8.5.2041.2060, threshold = threshold)
## 2070's
FLBC.SSP2.4.5.2061.2080CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP2.4.5.2061.2080, threshold = threshold)
FLBC.SSP3.7.0.2061.2080CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP3.7.0.2061.2080, threshold = threshold)
FLBC.SSP5.8.5.2061.2080CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP5.8.5.2061.2080, threshold = threshold)
FLMG.SSP2.4.5.2061.2080CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP2.4.5.2061.2080, threshold = threshold)
FLMG.SSP3.7.0.2061.2080CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP3.7.0.2061.2080, threshold = threshold)
FLMG.SSP5.8.5.2061.2080CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP5.8.5.2061.2080, threshold = threshold)
FLMC.SSP2.4.5.2061.2080CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP2.4.5.2061.2080, threshold = threshold)
FLMC.SSP3.7.0.2061.2080CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP3.7.0.2061.2080, threshold = threshold)
FLMC.SSP5.8.5.2061.2080CS <- centroidShift(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP5.8.5.2061.2080, threshold = threshold)

futures <- stack(list.files(path = , pattern = "\\.tif$", full.names=T))
futureCS <- lapply(futures, cenroidShift, currentProjection=FLcurrentSDM, futureProjection=futures, threshold=threshold)

###### Borders Crossed
threshold
boundaries
## 2050's
FLBC.SSP2.4.5.2041.2060BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP2.4.5.2041.2060, threshold = threshold, boundaries = boundaries)
FLBC.SSP3.7.0.2041.2060BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP3.7.0.2041.2060, threshold = threshold, boundaries = boundaries)
FLBC.SSP5.8.5.2041.2060BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP5.8.5.2041.2060, threshold = threshold, boundaries = boundaries)
FLMG.SSP2.4.5.2041.2060BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP2.4.5.2041.2060, threshold = threshold, boundaries = boundaries)
FLMG.SSP3.7.0.2041.2060BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP3.7.0.2041.2060, threshold = threshold, boundaries = boundaries)
FLMG.SSP5.8.5.2041.2060BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP5.8.5.2041.2060, threshold = threshold, boundaries = boundaries)
FLMC.SSP2.4.5.2041.2060BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP2.4.5.2041.2060, threshold = threshold, boundaries = boundaries)
FLMC.SSP3.7.0.2041.2060BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP3.7.0.2041.2060, threshold = threshold, boundaries = boundaries)
FLMC.SSP5.8.5.2041.2060BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP5.8.5.2041.2060, threshold = threshold, boundaries = boundaries)
## 2070's
FLBC.SSP2.4.5.2061.2080BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP2.4.5.2061.2080, threshold = threshold, boundaries = boundaries)
FLBC.SSP3.7.0.2061.2080BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP3.7.0.2061.2080, threshold = threshold, boundaries = boundaries)
FLBC.SSP5.8.5.2061.2080BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP5.8.5.2061.2080, threshold = threshold, boundaries = boundaries)
FLMG.SSP2.4.5.2061.2080BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP2.4.5.2061.2080, threshold = threshold, boundaries = boundaries)
FLMG.SSP3.7.0.2061.2080BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP3.7.0.2061.2080, threshold = threshold, boundaries = boundaries)
FLMG.SSP5.8.5.2061.2080BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP5.8.5.2061.2080, threshold = threshold, boundaries = boundaries)
FLMC.SSP2.4.5.2061.2080BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP2.4.5.2061.2080, threshold = threshold, boundaries = boundaries)
FLMC.SSP3.7.0.2061.2080BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP3.7.0.2061.2080, threshold = threshold, boundaries = boundaries)
FLMC.SSP5.8.5.2061.2080BC <- bordersChanged(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP5.8.5.2061.2080, threshold = threshold, boundaries = boundaries)

futures <- stack(list.files(path = , pattern = "\\.tif$", full.names=T))
futureBC <- lapply(futures, bordersChanged, currentProjection=FLCurrentSDM, futureProjection=futures, threshold=threshold, boundaries=boundaries)

###### Ordnance Crossed
threshold
UXO
UXOthreshold
## 2050's
FLBC.SSP2.4.5.2041.2060OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP2.4.5.2041.2060, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLBC.SSP3.7.0.2041.2060OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP3.7.0.2041.2060, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLBC.SSP5.8.5.2041.2060OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP5.8.5.2041.2060, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMG.SSP2.4.5.2041.2060OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP2.4.5.2041.2060, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMG.SSP3.7.0.2041.2060OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP3.7.0.2041.2060, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMG.SSP5.8.5.2041.2060OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP5.8.5.2041.2060, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMC.SSP2.4.5.2041.2060OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP2.4.5.2041.2060, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMC.SSP3.7.0.2041.2060OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP3.7.0.2041.2060, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMC.SSP5.8.5.2041.2060OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP5.8.5.2041.2060, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
## 2070's
FLBC.SSP2.4.5.2061.2080OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP2.4.5.2061.2080, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLBC.SSP3.7.0.2061.2080OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP3.7.0.2061.2080, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLBC.SSP5.8.5.2061.2080OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP5.8.5.2061.2080, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMG.SSP2.4.5.2061.2080OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP2.4.5.2061.2080, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMG.SSP3.7.0.2061.2080OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP3.7.0.2061.2080, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMG.SSP5.8.5.2061.2080OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP5.8.5.2061.2080, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMC.SSP2.4.5.2061.2080OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP2.4.5.2061.2080, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMC.SSP3.7.0.2061.2080OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP3.7.0.2061.2080, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)
FLMC.SSP5.8.5.2061.2080OC <- deltaUXO(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP5.8.5.2061.2080, threshold = threshold, UXO = UXO, UXOthreshold = UXOthreshold)

futures <- stack(list.files(path = , pattern = "\\.tif$", full.names=T))
futureOC <- lapply(futures, deltaUXO, currentProjection=FLCurrentSDM, futureProjection=futures, threshold=threshold, UXO=UXO, UXOthreshold=UXOthreshold)

###### Change in PA coverage
threshold
PA
## 2050's
FLBC.SSP2.4.5.2041.2060PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP2.4.5.2041.2060, threshold = threshold, PA = PA)
FLBC.SSP3.7.0.2041.2060PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP3.7.0.2041.2060, threshold = threshold, PA = PA)
FLBC.SSP5.8.5.2041.2060PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP5.8.5.2041.2060, threshold = threshold, PA = PA)
FLMG.SSP2.4.5.2041.2060PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP2.4.5.2041.2060, threshold = threshold, PA = PA)
FLMG.SSP3.7.0.2041.2060PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP3.7.0.2041.2060, threshold = threshold, PA = PA)
FLMG.SSP5.8.5.2041.2060PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP5.8.5.2041.2060, threshold = threshold, PA = PA)
FLMC.SSP2.4.5.2041.2060PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP2.4.5.2041.2060, threshold = threshold, PA = PA)
FLMC.SSP3.7.0.2041.2060PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP3.7.0.2041.2060, threshold = threshold, PA = PA)
FLMC.SSP5.8.5.2041.2060PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP5.8.5.2041.2060, threshold = threshold, PA = PA)
## 2070's
FLBC.SSP2.4.5.2061.2080PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP2.4.5.2061.2080, threshold = threshold, PA = PA)
FLBC.SSP3.7.0.2061.2080PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP3.7.0.2061.2080, threshold = threshold, PA = PA)
FLBC.SSP5.8.5.2061.2080PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLBC.SSP5.8.5.2061.2080, threshold = threshold, PA = PA)
FLMG.SSP2.4.5.2061.2080PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP2.4.5.2061.2080, threshold = threshold, PA = PA)
FLMG.SSP3.7.0.2061.2080PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP3.7.0.2061.2080, threshold = threshold, PA = PA)
FLMG.SSP5.8.5.2061.2080PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMG.SSP5.8.5.2061.2080, threshold = threshold, PA = PA)
FLMC.SSP2.4.5.2061.2080PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP2.4.5.2061.2080, threshold = threshold, PA = PA)
FLMC.SSP3.7.0.2061.2080PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP3.7.0.2061.2080, threshold = threshold, PA = PA)
FLMC.SSP5.8.5.2061.2080PAc <- PAareaChange(currentProjection = FLCurrentSDM, futureProjection = FLMC.SSP5.8.5.2061.2080, threshold = threshold, PA = PA)

futures <- stack(list.files(path = , pattern = "\\.tif$", full.names=T))
futurePAc <- lapply(futures, PAareaChange, currentProjection=FLCurrentSDM, futureProjection=futures, threshold=threshold, PA=PA)

###############################################
###############################################
########  Combine results into table  #########
FLTable <- cbind(rbind(c(FLBC.SSP2.4.5.2041.2060RAC,
                       FLBC.SSP3.7.0.2041.2060RAC,
                       FLBC.SSP5.8.5.2041.2060RAC,
                       FLMG.SSP2.4.5.2041.2060RAC,
                       FLMG.SSP3.7.0.2041.2060RAC,
                       FLMG.SSP5.8.5.2041.2060RAC,
                       FLMC.SSP2.4.5.2041.2060RAC,
                       FLMC.SSP3.7.0.2041.2060RAC,
                       FLMC.SSP5.8.5.2041.2060RAC,
                       FLBC.SSP2.4.5.2061.2080RAC,
                       FLBC.SSP3.7.0.2061.2080RAC,
                       FLBC.SSP5.8.5.2061.2080RAC,
                       FLMG.SSP2.4.5.2061.2080RAC,
                       FLMG.SSP3.7.0.2061.2080RAC,
                       FLMG.SSP5.8.5.2061.2080RAC,
                       FLMC.SSP2.4.5.2061.2080RAC,
                       FLMC.SSP3.7.0.2061.2080RAC,
                       FLMC.SSP5.8.5.2061.2080RAC)),
               rbind(c(FLBC.SSP2.4.5.2041.2060CS,
                       FLBC.SSP3.7.0.2041.2060CS,
                       FLBC.SSP5.8.5.2041.2060CS,
                       FLMG.SSP2.4.5.2041.2060CS,
                       FLMG.SSP3.7.0.2041.2060CS,
                       FLMG.SSP5.8.5.2041.2060CS,
                       FLMC.SSP2.4.5.2041.2060CS,
                       FLMC.SSP3.7.0.2041.2060CS,
                       FLMC.SSP5.8.5.2041.2060CS,
                       FLBC.SSP2.4.5.2061.2080CS,
                       FLBC.SSP3.7.0.2061.2080CS,
                       FLBC.SSP5.8.5.2061.2080CS,
                       FLMG.SSP2.4.5.2061.2080CS,
                       FLMG.SSP3.7.0.2061.2080CS,
                       FLMG.SSP5.8.5.2061.2080CS,
                       FLMC.SSP2.4.5.2061.2080CS,
                       FLMC.SSP3.7.0.2061.2080CS,
                       FLMC.SSP5.8.5.2061.2080CS)), 
               rbind(c(FLBC.SSP2.4.5.2041.2060BC,
                       FLBC.SSP3.7.0.2041.2060BC,
                       FLBC.SSP5.8.5.2041.2060BC,
                       FLMG.SSP2.4.5.2041.2060BC,
                       FLMG.SSP3.7.0.2041.2060BC,
                       FLMG.SSP5.8.5.2041.2060BC,
                       FLMC.SSP2.4.5.2041.2060BC,
                       FLMC.SSP3.7.0.2041.2060BC,
                       FLMC.SSP5.8.5.2041.2060BC,
                       FLBC.SSP2.4.5.2061.2080BC,
                       FLBC.SSP3.7.0.2061.2080BC,
                       FLBC.SSP5.8.5.2061.2080BC,
                       FLMG.SSP2.4.5.2061.2080BC,
                       FLMG.SSP3.7.0.2061.2080BC,
                       FLMG.SSP5.8.5.2061.2080BC,
                       FLMC.SSP2.4.5.2061.2080BC,
                       FLMC.SSP3.7.0.2061.2080BC,
                       FLMC.SSP5.8.5.2061.2080BC)),
               rbind(c(FLBC.SSP2.4.5.2041.2060OC,
                       FLBC.SSP3.7.0.2041.2060OC,
                       FLBC.SSP5.8.5.2041.2060OC,
                       FLMG.SSP2.4.5.2041.2060OC,
                       FLMG.SSP3.7.0.2041.2060OC,
                       FLMG.SSP5.8.5.2041.2060OC,
                       FLMC.SSP2.4.5.2041.2060OC,
                       FLMC.SSP3.7.0.2041.2060OC,
                       FLMC.SSP5.8.5.2041.2060OC,
                       FLBC.SSP2.4.5.2061.2080OC,
                       FLBC.SSP3.7.0.2061.2080OC,
                       FLBC.SSP5.8.5.2061.2080OC,
                       FLMG.SSP2.4.5.2061.2080OC,
                       FLMG.SSP3.7.0.2061.2080OC,
                       FLMG.SSP5.8.5.2061.2080OC,
                       FLMC.SSP2.4.5.2061.2080OC,
                       FLMC.SSP3.7.0.2061.2080OC,
                       FLMC.SSP5.8.5.2061.2080OC)),
               rbind(c(FLBC.SSP2.4.5.2041.2060PAc,
                       FLBC.SSP3.7.0.2041.2060PAc,
                       FLBC.SSP5.8.5.2041.2060PAc,
                       FLMG.SSP2.4.5.2041.2060PAc,
                       FLMG.SSP3.7.0.2041.2060PAc,
                       FLMG.SSP5.8.5.2041.2060PAc,
                       FLMC.SSP2.4.5.2041.2060PAc,
                       FLMC.SSP3.7.0.2041.2060PAc,
                       FLMC.SSP5.8.5.2041.2060PAc,
                       FLBC.SSP2.4.5.2061.2080PAc,
                       FLBC.SSP3.7.0.2061.2080PAc,
                       FLBC.SSP5.8.5.2061.2080PAc,
                       FLMG.SSP2.4.5.2061.2080PAc,
                       FLMG.SSP3.7.0.2061.2080PAc,
                       FLMG.SSP5.8.5.2061.2080PAc,
                       FLMC.SSP2.4.5.2061.2080PAc,
                       FLMC.SSP3.7.0.2061.2080PAc,
                       FLMC.SSP5.8.5.2061.2080PAc)))
colnames(FLTable) <- c("Range Area Change", "Centroid Shift", "Borders Crossed", "Ordinance Crossed", "Change in PA Coverage", "Change in proportion of range within PA")
row.names(FLTable) <- c(FLBC.SSP2.4.5.2041.2060,
                        FLBC.SSP3.7.0.2041.2060,
                        FLBC.SSP5.8.5.2041.2060,
                        FLMG.SSP2.4.5.2041.2060,
                        FLMG.SSP3.7.0.2041.2060,
                        FLMG.SSP5.8.5.2041.2060,
                        FLMC.SSP2.4.5.2041.2060,
                        FLMC.SSP3.7.0.2041.2060,
                        FLMC.SSP5.8.5.2041.2060,
                        FLBC.SSP2.4.5.2061.2080,
                        FLBC.SSP3.7.0.2061.2080,
                        FLBC.SSP5.8.5.2061.2080,
                        FLMG.SSP2.4.5.2061.2080,
                        FLMG.SSP3.7.0.2061.2080,
                        FLMG.SSP5.8.5.2061.2080,
                        FLMC.SSP2.4.5.2061.2080,
                        FLMC.SSP3.7.0.2061.2080,
                        FLMC.SSP5.8.5.2061.2080)
write.csv(FLTable, "FrancoisLangurMetrics.csv")
  
  
  