#=========================================================================
#=========================================================================
#########  Change in PA  ############
#' @title Area change in Protected areas
#' @description Calculate the areal change of current and future SDMs within protected areas
#' @param currentProjection raster object of SDM from current time period
#' @param futureProjection raster object of SDM from future time period
#' @param threshold a float value of the acceptable threshold for the current projection to make the SDM into a binary
#' @param PA a simple feature object of a shapefile showing protected areas in SEA
#' @return a data.frame showing values in meter of: area change, current percent protected, future percent protected, percent change.
#' @author Peter Galante <pgalante@@amnh.org>
#TESTING
# PA <- st_read("C:/Users/pgalante/layers/VN_Protected_Areas", "VN_NRs")
# currentProjection <- raster('C:/Users/pgalante/Projects/Vietnam/FrancoisiLangur/Reliable_locs/ClimateKarstUXO/climateKarstUXO.tif')
# futureProjection <- raster('C:/Users/pgalante/Projects/Vietnam/FrancoisiLangur/Reliable_locs/ClimateOnly/climateOnly.tif')
# threshold <- 0.5
# PAareaChange(currentProjection, futureProjection, threshold, PA)

PAareaChange <- function(currentProjection, futureProjection, threshold, PA){
  require(raster)
  require(geosphere)
  require(sf)
  require(rgeos)
  options(warn = -1)
  # Check that CRS is the same for rasters and shapefile. If not, reproject rasters to match
  sameCRS <- raster::compareCRS(raster::crs(PA), raster::crs(currentProjection))
  if (!sameCRS){
    cat("Projecting rasters to match shapefile - this can take a while if the projections are large\n")
    currentProjection <- raster::projectRaster(currentProjection, crs(PA), method = "bilinear")
    futureProjection <- raster::projectRaster(futureProjection, crs(PA), method = "bilinear")
  }
  # current model to binary
  currentProjection[currentProjection < threshold] <- NA
  currentProjection[currentProjection >= threshold] <- 1
  # set CRS
  raster::crs(futureProjection) <- raster::crs(currentProjection)
  # Convert current to poly
  currentPoly <-raster::rasterToPolygons(currentProjection, fun = NULL, dissolve = T)
  # future model to binary
  futureProjection[futureProjection < threshold] <- NA
  futureProjection[futureProjection >= threshold] <- 1
  # Convert future to poly
  futurePoly <- raster::rasterToPolygons(futureProjection, fun = NULL, dissolve = T)
  # Clean up geometries
  currentPoly <- rgeos::gBuffer(currentPoly, byid=TRUE, width=0)
  futurePoly <- rgeos::gBuffer(futurePoly, byid=TRUE, width=0)
  PA <- sf::st_buffer(PA, 0)
  # convert to sf object
  currentPoly <- as(currentPoly, "sf")
  futurePoly <- as(futurePoly, "sf")
  # for current and future, find intersections with PA
  currentArea <- st_intersection(currentPoly, PA) %>% st_area()
  futureArea <- st_intersection(futurePoly, PA) %>% st_area()
  # Calculate the difference in areas
  areaDiffs <- currentArea - futureArea
  areaChange <- sum(areaDiffs)
  # Calculate pecentage protected of total predicted area
  curPercent <- (sum(currentArea) / sf::st_area(currentPoly)) * 100
  futPercent <- (sum(futureArea) / sf::st_area(futurePoly)) * 100

  areaChangeTab <- cbind(areaChange, curPercent, futPercent, (curPercent - futPercent))
  colnames(areaChangeTab) <- c("area change", "current proportion protected", "future proportion protected", "percent change")
  options(warn = 0)
  return(areaChangeTab)
}

#########  Crossed UXO?  ############
#' @title Crossing rasterized barriers
#' @description Calculate the number of times a current and future binary model crosses a binary barrier layer.
#' @param currentProjection raster object of a continuous SDM from current time period
#' @param futureProjection raster object of a continuous SDM from future time period
#' @param threshold a float value of the acceptable threshold for the current projection to make the SDM into a binary
#' @param UXOthreshold a float value to turn the continuous UXO kernel density estimate into a binary map. Default value it 0.25
#' @return matrix showing the current, future, and change in number of times the binary model crossed the barrier
#' @author Peter Galante <pgalante@@amnh.org>
# TESTING
# library(sf)
# library(raster)
# currentProjection <- raster('C:/Users/pgalante/Projects/Vietnam/FrancoisiLangur/Reliable_locs/ClimateKarstUXO/climateKarstUXO.tif')
# futureProjection <- raster('C:/Users/pgalante/Projects/Vietnam/FrancoisiLangur/Reliable_locs/ClimateOnly/climateOnly.tif')
# threshold <- 0.5
# uxo <- raster('C:/Users/pgalante/Projects/Vietnam/USAF_Bombing_database/KerDen/USAF_KDE.tif')
# UXOthreshold <- 0.25
# deltaUXO(currentProjection, futureProjection, threshold, UXOthreshold)
 
deltaUXO <- function(currentProjection, futureProjection, threshold, UXOthreshold = 0.25){
  require(sf)
  require(raster)
  options(warn = -1)
  
  sameCRS <- raster::compareCRS(raster::crs(currentProjection), raster::crs(futureProjection), raster::crs(uxo))
  if (!sameCRS){
    cat("Projecting rasters to match shapefile - this can take a while if the projections are large\n")
    currentProjection <- raster::projectRaster(currentProjection, crs(uxo), method = "bilinear")
    futureProjection <- raster::projectRaster(futureProjection, crs(uxo), method = "bilinear")
  }
  
  # Threshold KDE into binary
  uxo[uxo < UXOthreshold] <- NA
  uxo[uxo >= UXOthreshold] <- 1
  # convert KDE binary to polygon
  uxoPoly <- as(raster::rasterToPolygons(uxo, fun = NULL, dissolve = T), "sf")
  # Threshold current projection to binary and ensure projections are same
  currentProjection[currentProjection < threshold] <- NA
  currentProjection[currentProjection >= threshold] <- 1
  raster::crs(futureProjection) <- raster::crs(currentProjection)
  # Convert current pred to polygon
  currentPoly <-raster::rasterToPolygons(currentProjection, fun = NULL, dissolve = T)
  # threhold future projection to binary and convert to polygon
  futureProjection[futureProjection < threshold] <- NA
  futureProjection[futureProjection >= threshold] <- 1
  futurePoly <- raster::rasterToPolygons(futureProjection, fun = NULL, dissolve = T)
  # Convert current and future polygons to simple features objects
  currentPoly <- as(currentPoly, "sf")
  futurePoly <- as(futurePoly, "sf")
  # Calculate the number of intersections
  currentUXO <- sf::st_intersection(currentPoly, uxoPoly)
  futureUXO <- sf::st_intersection(futurePoly, uxoPoly)
  uxoChange <- nrow(currentUXO) - nrow(futureUXO)
  
  uxoCrossed <- cbind(nrow(currentUXO), nrow(futureUXO), uxoChange)
  colnames(uxoCrossed) <- c("current # times", "future # times", "change")
  options(warn=0)
  return(uxoCrossed)
}

#########  Crossed border?  #########
#' @title SDM borders crossed 
#' @description Calculate the number of borders crossed in current and future SDMs
#' @param currentProjection raster object of SDM from current time period
#' @param futureProjection raster object of SDM from future time period
#' @param threshold a float value of the acceptable threshold for the current projection to make the SDM into a binary
#' @param boundaries a shapefile of administratice boundaries of interest for range crossings 
#' @return a matrix of current boundaries crossed, future boundaries crossed, and the change
#' @author Peter Galante <pgalante@@amnh.org>
# TESTING
# library(raster)
# library(sf)
# currentProjection <- raster('C:/Users/pgalante/Projects/Vietnam/FrancoisiLangur/Reliable_locs/ClimateKarstUXO/climateKarstUXO.tif')
# futureProjection <- raster('C:/Users/pgalante/Projects/Vietnam/FrancoisiLangur/Reliable_locs/ClimateOnly/climateOnly.tif')
# threshold <- 0.5
# boundaries <- st_read("C:/Users/pgalante/layers/ne_50m_countries", "ne_50m_admin_0_countries")
# bordersChanged(currentProjection, futureProjection, threshold, boundaries)

bordersChanged <- function(currentProjection, futureProjection, threshold, boundaries){
  require(raster)
  require(sf)
  options(warn = -1)
  # turn projections to binary
  currentProjection[currentProjection < threshold] <- NA
  currentProjection[currentProjection >= threshold] <- 1
  futureProjection[futureProjection < threshold] <- NA
  futureProjection[futureProjection >= threshold] <- 1
  # Set CRS and convert to polygon
  raster::crs(futureProjection) <- raster::crs(currentProjection)
  currentPoly <-raster::rasterToPolygons(currentProjection, fun = NULL, dissolve = T)
  futurePoly <- raster::rasterToPolygons(futureProjection, fun = NULL, dissolve = T)
  # convert to SF objects
  currentPoly <- as(currentPoly, "sf")
  futurePoly <- as(futurePoly, "sf")
  # Calculate number of intersections for each time period and get difference
  currentInts <- sf::st_intersection(currentPoly, boundaries)
  futureInts <-  sf::st_intersection(futurePoly, boundaries)
  deltaBords <- nrow(currentInts) - nrow(futureInts)
  
  bordsChanged <- cbind(nrow(currentInts), nrow(futureInts), deltaBords)
  colnames(bordsChanged) <- c("current boundaries crossed", "future boundaries crossed", "change")
  options(warn = 0)
  return(bordsChanged)
}

#########  CENTROID SHIFT  #########
#' @title Calculate the shift in SDM centroids 
#' @description Calculate the distance between current and future SDM centroids
#' @param currentProjection raster object of SDM from current time period in WGS84 projection
#' @param futureProjection raster object of SDM from future time period WGS84 projection
#' @param threshold a float value of the acceptable threshold for the current projection to make the SDM into a binary
#' @author Peter Galante <pgalante@@amnh.org>
#' @return a float value of the distance in meters
# TESTING
# library(raster)
# library(geosphere)
# currentProjection <- raster('C:/Users/pgalante/Projects/Vietnam/FrancoisiLangur/Reliable_locs/ClimateKarstUXO/climateKarstUXO.tif')
# futureProjection <- raster('C:/Users/pgalante/Projects/Vietnam/FrancoisiLangur/Reliable_locs/ClimateOnly/climateOnly.tif')
# threshold <- 0.5
# centroidShift(currentProjection, futureProjection, threshold)

centroidShift <- function(currentProjection, futureProjection, threshold){
    require(raster)
    require(geosphere)
    # Threshold projections to binary 
    currentProjection[currentProjection < threshold] <- NA
    currentProjection[currentProjection >= threshold] <- 1
    futureProjection[futureProjection < threshold] <- NA
    futureProjection[futureProjection >= threshold] <- 1
    # Convert binary maps to polygons
    currentPoly <-raster::rasterToPolygons(currentProjection, fun = NULL, dissolve = T)
    futurePoly <- raster::rasterToPolygons(futureProjection, dissolve = T)
    # Calculate distance between centroids
    dist <- raster::pointDistance(geosphere::centroid(currentPoly), geosphere::centroid(futurePoly), lonlat = TRUE)
    return(dist)
}

#########  RANGE AREA CHANGE  #########
#' @title Calculate the change in range areas
#' @description Calculate the change in areas of two different SDMs given a threshold. Ideally using a current and future projection of the same model
#' @param currentProjection raster object of SDM from current time period
#' @param futureProjection raster object of SDM from future time period
#' @param threshold a float value of the acceptable threshold for the current projection to make the SDM into a binary
#' @param projection boolean for whether or not the SDMs are projected. Default is not. Projection will make the range area calculation more accurate.
#' @author Peter Galante <pgalante@@amnh.org>
#' @return a float value of the area in either km^2 if rasters are not projected, and in projection units if they are projected.
# library(raster)
# currentProjection <- raster('C:/Users/pgalante/Projects/Vietnam/FrancoisiLangur/Reliable_locs/ClimateKarstUXO/climateKarstUXO.tif')
# futureProjection <- raster('C:/Users/pgalante/Projects/Vietnam/FrancoisiLangur/Reliable_locs/ClimateOnly/climateOnly.tif')
# threshold <- 0.5
# projection = F
# rangeAreaChange(currentProjection, futureProjection, threshold, projection)

rangeAreaChange <- function(currentProjection, futureProjection, threshold, projection = F){
    require(raster)
    # Threshold projections at minimum projection (not binary yet)
    currentProjection[currentProjection > threshold] <- NA
    futureProjection[futureProjection > threshold] <- NA
    # If the rasters are not projected, calculate the difference in km^2 estimated based on WGS84 ellipsoid
    if (is.null(projection)){
    areaChange <- sum(raster::values(raster::area(currentProjection, na.rm=T)), na.rm=T) - sum(raster::values(raster::area(futureProjection, na.rm=T)), na.rm=T)
    return(areaChange)
    } else {
    # If SDMs are projected, calculate based on the defines projection
    areaChange <- sum(raster::values(raster::area(currentProjection, na.rm=T, projection = projection)), na.rm=T) - sum(raster::values(raster::area(futureProjection, na.rm=T, projection = projection)), na.rm=T)
    deltArea <- cbind(sum(raster::values(raster::area(currentProjection, na.rm=T, projection = projection)), na.rm=T), sum(raster::values(raster::area(futureProjection, na.rm=T, projection = projection)), na.rm=T), areaChange)
    colnames(deltArea) <- c("Current area", "Future area", "Area change")
    return(areaChange)
    }
}
