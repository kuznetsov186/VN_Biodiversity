library(raster)
library(geosphere)
library(sf)
#########  Change in PA  ############
#' @param currentProjection raster object of SDM from current time period
#' @param futureProjection raster object of SDM from future time period
#' @param threshold a float value of the acceptable threshold for the current projection to make the SDM into a binary
#' @param PA a shapefile showing protected areas in SEA
#TESTING
# PA <- st_read("/home/pgalante/Projects/Vietnam/Protected_areas/VN Protected Areas", "VN_NRs")
# currentProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LIGCan.tif')
# futureProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LGMCan.tif')
# threshold <- 0.5

PAareaChange <- function(currentProjection, futureProjection, threshold, PA){
  require(raster)
  require(sf)
  currentProjection[currentProjection < threshold] <- NA
  currentProjection[currentProjection >= threshold] <- 1
  crs(futureProjection) <- crs(currentProjection)
  currentPoly <-rasterToPolygons(currentProjection, fun = NULL, dissolve = T)
  futureProjection[futureProjection < threshold] <- NA
  futureProjection[futureProjection >= threshold] <- 1
  futurePoly <- rasterToPolygons(futureProjection, fun = NULL, dissolve = T)
  currentPoly <- as(currentPoly, "sf")
  futurePoly <- as(futurePoly, "sf")
  currentArea <- st_intersection(currentPoly, PA) %>% st_area()
  futureArea <- st_intersection(currentPoly, PA) %>% st_area()
  PAareaChange <- currentArea - futureArea
  areaChangeTab <- cbind(currentArea, futureArea, PAareaChange)
  return(areaChangeTab)
}

#########  Crossed UXO?  ############
#' @param currentProjection raster object of SDM from current time period
#' @param futureProjection raster object of SDM from future time period
#' @param threshold a float value of the acceptable threshold for the current projection to make the SDM into a binary
#' @param UXOthreshold a float value to turn the UXO kernel density estimate into a binary map. Default value it 0.25
# TESTING
# currentProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LIGCan.tif')
# futureProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LGMCan.tif')
# threshold <- 0.5
# uxo <- raster('/home/pgalante/Projects/Vietnam/USAF_Bombing_database/KerDen/USAF_KDE.tif')
# UXOthreshold <- 0.25
 
deltaUXO <- function(currentProjection, futureProjection, threshold, UXOthreshold = 0.25){
  require(sf)
  require(raster)
  uxo[uxo < UXOthreshold] <- NA
  uxo[uxo >= UXOthreshold] <- 1
  uxoPoly <- as(rasterToPolygons(uxo, fun = NULL, dissolve = T), "sf")
  currentProjection[currentProjection < threshold] <- NA
  currentProjection[currentProjection >= threshold] <- 1
  crs(futureProjection) <- crs(currentProjection)
  currentPoly <-rasterToPolygons(currentProjection, fun = NULL, dissolve = T)
  futureProjection[futureProjection < threshold] <- NA
  futureProjection[futureProjection >= threshold] <- 1
  futurePoly <- rasterToPolygons(futureProjection, fun = NULL, dissolve = T)
  currentPoly <- as(currentPoly, "sf")
  futurePoly <- as(futurePoly, "sf")
  currentUXO <- st_intersection(currentPoly, uxoPoly)
  futureUXO <- st_intersection(futurePoly, uxoPoly)
  uxoChange <- nrow(currentUXO) - nrow(futureUXO)
  return(uxoChange)
}

#########  Crossed border?  #########
#' @param currentProjection raster object of SDM from current time period
#' @param futureProjection raster object of SDM from future time period
#' @param threshold a float value of the acceptable threshold for the current projection to make the SDM into a binary
#' @param boundaries a shapefile of administratice boundaries of interest for range crossings 
# TESTING
# currentProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LIGCan.tif')
# futureProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LGMCan.tif')
# threshold <- 0.5
# boundaries <- st_read("/home/pgalante/layers/countries", "ne_50m_admin_0_countries")

bordersChanged <- function(currentProjection, futureProjection, threshold, boundaries){
  require(raster)
  require(geosphere)
  require(sf)
  currentProjection[currentProjection < threshold] <- NA
  currentProjection[currentProjection >= threshold] <- 1
  crs(futureProjection) <- crs(currentProjection)
  currentPoly <-rasterToPolygons(currentProjection, fun = NULL, dissolve = T)
  futureProjection[futureProjection < threshold] <- NA
  futureProjection[futureProjection >= threshold] <- 1
  futurePoly <- rasterToPolygons(futureProjection, fun = NULL, dissolve = T)
  currentPoly <- as(currentPoly, "sf")
  futurePoly <- as(futurePoly, "sf")
  currentInts <- st_intersection(currentPoly, boundaries)
  futureInts <-  st_intersection(futurePoly, boundaries)
  deltaBords <- nrow(currentInts) - nrow(futureInts)
  return(deltaBords)
}

#########  CENTROID SHIFT  #########
#' @param currentProjection raster object of SDM from current time period
#' @param futureProjection raster object of SDM from future time period
#' @param threshold a float value of the acceptable threshold for the current projection to make the SDM into a binary
# TESTING
# currentProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LIGCan.tif')
# futureProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LGMCan.tif')
# threshold <- 0.5

centroidShift <- function(currentProjection, futureProjection, threshold){
    require(raster)
    require(geosphere)
    currentProjection[currentProjection < threshold] <- NA
    currentProjection[currentProjection >= threshold] <- 1
    currentPoly <-rasterToPolygons(currentProjection, fun = NULL, dissolve = T)
    futureProjection[futureProjection < threshold] <- NA
    futureProjection[futureProjection >= threshold] <- 1
    futurePoly <- rasterToPolygons(futureProjection, dissolve = T)
    dist <- pointDistance(centroid(currentPoly), centroid(futurePoly), lonlat = TRUE)
    return(dist)
}

#########  RANGE AREA CHANGE  #########
#' @param currentProjection raster object of SDM from current time period
#' @param futureProjection raster object of SDM from future time period
#' @param threshold a float value of the acceptable threshold for the current projection to make the SDM into a binary
#' @param projection boolean for whether or not the SDMs are projected. Default is not. Projection will make the range area calculation more accurate.
# currentProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LIGCan.tif')
# futureProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LGMCan.tif')
# threshold <- 0.5

rangeAreaChange <- function(currentProjection, futureProjection, threshold, projection = F){
    currentProjection[currentProjection > threshold] <- NA
    futureProjection[futureProjection > threshold] <- NA
    if (is.null(projection)){
    areaChange <- sum(values(area(currentProjection, na.rm=T)), na.rm=T) - sum(values(area(futureProjection, na.rm=T)), na.rm=T)
    return(areaChange)
    } else {
    areaChange <- sum(values(area(currentProjection, na.rm=T, projection = projection)), na.rm=T) - sum(values(area(futureProjection, na.rm=T, projection = projection)), na.rm=T)
    return(areaChange)
    }
}
