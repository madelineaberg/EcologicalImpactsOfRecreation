#### Recreation Intensity Functions ####
     #### November 19, 2021 ####

#### UPDATE: Feb 2024
  # the package 'maptools' has been archived
install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")
library(maptools)

#### Final Workflow Goals ####
# 1 - Load Packages
# 2 - Load Spatial & Tabular Data * tab data should go through CleanTabData.R first * 
# 3 - Make 2 versions of each necessary spatial data - Lat/Long & UTM 
# 4 - Join driving (lat/long) & cleaned tabular data & filter by observation type (all, rec, shoot, hunt, target)
# 5 - Subset driving data by time periods **** 
# 6 - Adjust point locations (PointAdjust function + need to make iterative)
# 7 - Kernel density map - raster- extract - save extraction values in a dataframe ****

#### adjust points function ####
# This function is using the distance and bearing we recorded during driving routes to fix the pt locations
# Before using, make sure that distance and bearing are numeric variables
# Just need to input a dataframe that is an sf object in Lat/Long CRS w/ bearing & distance & geometry info

PointAdjust <- function(df){ # df needs to be sf object, CRS = LatLong, WGS84
  library(sf)
  library(sp)
  library(geosphere)
  sp <- as_Spatial(df) # Make into a spatial object, so we can use the package
  adjust <- SpatialPoints(destPoint(sp, df$bearing, df$distance)) # Adjust points w/ distance & bearing
  proj4string(adjust) <- proj4string(sp) # Add CRS back to points
  return(adjust) # returns the adjusted points 
}

#test_PA <- PointAdjust(driving_all_np) # seems to be working!! 
#test_PA

#### Kernel density - Extraction Function ####

KD_extract <- function(W, P, d, E, b){
  # W = window or boundary for the extraction (sf object, NAD83/UTM zone 11N)
  # P = driving observation points (Spatial Points, Lat/Long WGS84) * should be adjusted points from PointAdjust()
  # d = distance in meters for the sigma of the kernel density (have been using 1000)
  # E = points to use in the raster extraction (paired sites, nests) - sf object, NAD83 UTM zone 11
  # b = buffer size for raster extraction in meters
  library(sf)
  library(spatstat)
  library(raster)
  library(maptools)
  library(rgdal)
  W.sp <- as_Spatial(W) # boundary - make a spatial object
  WW <- as(W.sp, "owin") # boundary - set as "owin" to match package format
  P.proj <- spTransform(P, proj4string(W.sp)) # project to CRS of the boundary
  P.proj.ppp <- as(P.proj, "ppp") # make "ppp" to match package format
  marks(P.proj.ppp) <- NULL # set all marks to null
  Window(P.proj.ppp) <- WW # define the study area
  KD <- density(P.proj.ppp, sigma = d) # kernel density
  plot(KD)
  sp_dat <- as.data.frame(KD) # store kernel density as a dataframe
  kd_raster <- rasterFromXYZ(sp_dat, crs = crs(W.sp)) # make kernel density dataframe into a raster
  # extract kernel density raster values with in b meters of E (paired site, nest)
  raster_extract <- raster::extract(kd_raster, as(E, "Spatial"), buffer = b, fun = mean)  
  return(raster_extract)
}


#### Kernel density - extraction function + trying to save raster ####

KD_extractsave <- function(W, P, d, E, b){
  # W = window or boundary for the extraction (sf object, NAD83/UTM zone 11N)
  # P = driving observation points (Spatial Points, Lat/Long WGS84) * should be adjusted points from PointAdjust()
  # d = distance in meters for the sigma of the kernel density (have been using 1000)
  # E = points to use in the raster extraction (paired sites, nests) - sf object, NAD83 UTM zone 11
  # b = buffer size for raster extraction in meters
  library(sf)
  library(spatstat)
  library(raster)
  library(maptools)
  W.sp <- as_Spatial(W) # boundary - make a spatial object
  WW <- as(W.sp, "owin") # boundary - set as "owin" to match package format
  P.proj <- spTransform(P, proj4string(W.sp)) # project to CRS of the boundary
  P.proj.ppp <- as(P.proj, "ppp") # make "ppp" to match package format
  marks(P.proj.ppp) <- NULL # set all marks to null
  Window(P.proj.ppp) <- WW # define the study area
  KD <- density(P.proj.ppp, sigma = d) # kernel density
  plot(KD)
  sp_dat <- as.data.frame(KD) # store kernel density as a dataframe
  kd_raster <- rasterFromXYZ(sp_dat, crs = crs(W.sp)) # make kernel density dataframe into a raster
  
  # save the raster in the DrivingRasters folder
  f <- paste0('raster', 99)
  writeRaster(kd_raster, filename = file.path("C:/Users/Madeline/Desktop/EcologicalImpactsManuscript", f), format = "GTiff", overwrite = TRUE)
  
  # extract kernel density raster values with in b meters of E (paired site, nest)
  raster_extract <- raster::extract(kd_raster, as(E, "Spatial"), buffer = b, fun = mean)  
  return(raster_extract)
}


#############################
#### Kernel density - extraction function + trying to save raster - splitting up ####
## don't like thissssss
# currently the same as KD_extractsave
KD_funct <- function(W, P, d, E, b){
  # W = window or boundary for the extraction (sf object, NAD83/UTM zone 11N)
  # P = driving observation points (Spatial Points, Lat/Long WGS84) * should be adjusted points from PointAdjust()
  # d = distance in meters for the sigma of the kernel density (have been using 1000)
  # E = points to use in the raster extraction (paired sites, nests) - sf object, NAD83 UTM zone 11
  # b = buffer size for raster extraction in meters
  library(sf)
  library(spatstat)
  library(raster)
  library(maptools)
  W.sp <- as_Spatial(W) # boundary - make a spatial object
  WW <- as(W.sp, "owin") # boundary - set as "owin" to match package format
  P.proj <- spTransform(P, proj4string(W.sp)) # project to CRS of the boundary
  P.proj.ppp <- as(P.proj, "ppp") # make "ppp" to match package format
  marks(P.proj.ppp) <- NULL # set all marks to null
  Window(P.proj.ppp) <- WW # define the study area
  KD <- density(P.proj.ppp, sigma = d) # kernel density
  plot(KD)
  sp_dat <- as.data.frame(KD) # store kernel density as a dataframe
  kd_raster <- rasterFromXYZ(sp_dat, crs = crs(W.sp)) # make kernel density dataframe into a raster
  
  # save the raster in the DrivingRasters folder
  f <- paste0('raster', 99, '.tif')
  writeRaster(kd_raster, filename = file.path("C:/Users/Madeline/Desktop/EcologicalImpactsManuscript", f))
  
  # extract kernel density raster values with in b meters of E (paired site, nest)
  raster_extract <- raster::extract(kd_raster, as(E, "Spatial"), buffer = b, fun = mean)  
  return(raster_extract)
}

#KD_extractsave(NCA_utm, shoot.adj, 1000, PairedSites_utm, 2000)

#e <- paste0('month', 2, '.tif')
#file.rename(paste0("/Users/madelineaberg/Desktop/Driving Analysis/DrivingRasters/", "raster99.tif"),
         #   paste0("/Users/madelineaberg/Desktop/Driving Analysis/DrivingRasters/", e))



extract_save <- function(KD) { 
  plot(KD)
  sp_dat <- as.data.frame(KD) # store kernel density as a dataframe
  kd_raster <- rasterFromXYZ(sp_dat, crs = crs(W.sp)) # make kernel density dataframe into a raster
  
  # save the raster in the DrivingRasters folder
  f <- paste0('raster', i, '.tif')
  writeRaster(kd_raster, filename = file.path("C:/Users/Madeline/Desktop/EcologicalImpactsManuscript", f))
  
  # extract kernel density raster values with in b meters of E (paired site, nest)
  raster_extract <- raster::extract(kd_raster, as(E, "Spatial"), buffer = b, fun = mean)  
  return(raster_extract)
}

#KD_funct(NCA_utm, shoot.adj, 1000, PairedSites_utm, 2000)
#file.rename(paste0(my_path, file_names_old),       # Rename files
 #           paste0(my_path, file_names_new))

