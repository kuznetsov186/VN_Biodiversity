library(raster)

currentProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LIGCan.tif')
futureProjection <- raster('/home/pgalante/Projects/LukeM/canescens/CanescensModel/LGMCan.tif')
threshold <- 0.5

test <- area(currentProjection, na.rm=T)
sum(values(test), na.rm=T)

rangeAreaChange <- function(currentProjection, futureProjection, threshold, projection = NULL){
    if (!is.null(projection){
    currentProjection[currentProjection > threshold] <- NA
    futureProjection[futureProjection > threshold] <- NA
    areaChange <- sum(values(area(currentProjection, na.rm=T)), na.rm=T) - sum(values(area(futureProjection, na.rm=T)), na.rm=T)
    return(areaChange)
    } else {
    currentProjection[currentProjection > threshold] <- NA
    futureProjection[futureProjection > threshold] <- NA
    areaChange <- sum(values(area(currentProjection, na.rm=T, projection = projection)), na.rm=T) - sum(values(area(futureProjection, na.rm=T, projection = projection)), na.rm=T)
    return(areaChange)
    }
}
test <- rangeAreaChange(currentProjection = currentProjection, futureProjection = futureProjection, threshold = threshold)
