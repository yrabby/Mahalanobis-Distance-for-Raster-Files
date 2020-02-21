##Mahalanobis Dustance

setwd("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance")
library(ggplot2)
library(dplyr)
data=read.csv("Mahalanobis_Landslides.csv")
MD= mahalanobis(data [, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)], colMeans(data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]), cov(data[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
data$MD=round(MD, 3)
mean(MD)
head(data)
write.csv(data, file = "data_MD.csv")




##Mahalanobis Distance for Raster Files
##Insatall Packages
install.packages("raster")
install.packages("rmarkdown")
library(rmarkdown)
library(raster)
install.packages("rgeos")
library(rgeos)
install.packages("rgdal")
library(rgdal)
install.packages("maptools")
library(maptools)
## Loading raster files
list.files("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance")

Elevation= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Elevation.tif")
Landuse= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Landuse.tif")
Change= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Change.tif")
Geology= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Geology.tif")
TWI= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distancet/TWI.tif")
SPI= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/SPI.tif")
Slope= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/slope.tif")
Road= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Road.tif")
Faultlines= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Faultlines.tif")
Profile= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Profile.tif")
Plan= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Plan.tif")
NDVI= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/NDVI.tif")
Drainage= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Drainage.tif")
Aspect= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Aspect.tif")
TWI= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/TWI.tif")
Rainfall= raster("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Rainfall.tif")


##Resampling Data
##Resampling

Landuse_re= resample(Landuse,Elevation, resmaple= 'bilinear')
Change_re= resample(Change,Elevation, resmaple= 'bilinear')
Geology_re= resample(Geology,Elevation, resmaple= 'bilinear')
TWI_re= resample(TWI,Elevation, resmaple= 'bilinear')
SPI_re= resample(SPI,Elevation, resmaple= 'bilinear')
Slope_re= resample(Slope,Elevation, resmaple= 'bilinear')
Road_re= resample(Road,Elevation, resmaple= 'bilinear')
Faultlines_re= resample(Faultlines,Elevation, resmaple= 'bilinear')
Profile_re= resample(Profile,Elevation, resmaple= 'bilinear')
Plan_re= resample(Plan,Elevation, resmaple= 'bilinear')
NDVI_re= resample(NDVI,Elevation, resmaple= 'bilinear')
Drainage_re= resample(Drainage,Elevation, resmaple= 'bilinear')
Aspect_re= resample(Aspect,Elevation, resmaple= 'bilinear')
Rainfall_re= resample(Rainfall,Elevation, resmaple= 'bilinear')
plot(Rainfall_re)



##Create Resample Raster
dir.create("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Resampled data")
setwd("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Resampled data")

writeRaster(Landuse_re, "Landuse_re.tiff", overwrite= TRUE)
writeRaster(Change_re, "Change_re.tiff", overwrite= TRUE)
writeRaster(Plan_re, "Plan_re.tiff", overwrite= TRUE)
writeRaster(Profile_re, "Profile_re.tiff", overwrite= TRUE)
writeRaster(NDVI_re, "NDVI_re.tiff", overwrite= TRUE)
writeRaster(Slope_re, "slope_re.tiff", overwrite= TRUE)
writeRaster(Road_re, "Road_re.tiff", overwrite= TRUE)
writeRaster(SPI_re, "SPI_re.tiff", overwrite= TRUE)
writeRaster(TWI_re, "TWI_re.tiff", overwrite= TRUE)
writeRaster(Geology_re, "Geology_re.tiff", overwrite= TRUE)
writeRaster(Faultlines_re, "Faultlines_re.tiff", overwrite= TRUE)
writeRaster(Drainage_re, "Drainage_re.tiff", overwrite= TRUE)
writeRaster(Aspect_re, "Aspect_re.tiff", overwrite= TRUE)
writeRaster(Elevation, "Elevation.tiff", overwrite= TRUE)
writeRaster(Rainfall_re, "Rainfall_re.tiff", overwrite= TRUE)
list.files("Resampled Data")


Stack_List= list.files("C:/Rangamati/Corrected Rangamati/Sampling Based on Slope/Mahalanobis Distance/Resampled data", pattern= "tif$", full.names= TRUE)
Rasters= stack(Stack_List)
names(Rasters)
head(Rasters)
value_table= getValues(Rasters)
head(value_table, n=6)
value_table= na.omit(value_table)
value_table= as.data.frame(value_table)
head(value_table, n=600)
write.csv(value_table, file = "data_MD.csv")

##Mahalanobis Distance Calculator
library(ggplot2)
library(dplyr)
data=read.csv("data_MD.csv")

MD= mahalanobis(data [, c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)], colMeans(data[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]), cov(data[, c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]))
data$MD=round(MD, 3)
mean(MD)
head(data)
write.csv(data, file = "data_MD1.csv")
data=read.csv("data_MD1.csv")
data2=read.csv("data_MD1.csv", sep= " ")[ ,c('col2', 'col18')]
install.packages("sf")
library(sf)
