# Module for interpolating air quality maps
# Copyright (C) 2017 William Schuch
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Check for required packages and install them (incl dependencies) if they are not installed yet.
list.of.packages <- c("sp","rgdal", "tmap", "gstat")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(sp)

PointsToRaster <- function(CT.SPDF, ...)
{
  Temp_dir = file.path("..", "output", "temp")
  if (!dir.exists(Temp_dir)) 
  {
    dir.create(Temp_dir)
  }
  
  activeDataset = NA
  activeLocation = NA
  activeName = NA
  for (p in seq_along(CT.SPDF))
  {
    activeDataset[p] = CT.SPDF@data[p,1] / 10000
    activeLocation[p] = CT.SPDF@data[p,1] %% 10000
    activeName[p] = activeDataset[p] - (activeLocation[p]/10000)
  }
  
  slots = seq(0,42) # find nicer way (auto slot detection on IDF5-file)
  usedNames = slots %in% activeName
  usedSlots = slots[usedNames]
  
  EXP = CT.SPDF # copy ConversionTable to fill in the EXP values
  EXP@data$hour19 = NA
  
  for (a in usedSlots)
  #for (a in seq(0,20))
    #for (a in head(usedSlots,3))
  {
    print(paste("Starting with slot", a, "of", length(usedSlots)))
    
    for (p in seq_along(CT.SPDF))
    {
      if (activeName[p] == as.character(a))
      {
        File = file.path(Temp_dir, paste0(paste("DF",activeName[p], activeLocation[p], sep = "-"), ".dbf"))
        if (file.exists(File))
        {
          print(paste0(File, " exists. reading it..."))
          DF = read.dbf(File)
        }else{
          print(paste(File, "does not exist. reading h5f slot", a, "..."))
          h5f.active = h5read(h5f_dir, as.character(a))
          
          DF = data.frame(h5f.active$data[,activeLocation[p]])
          SaveAsDBF(DF, paste("DF", activeName[p], activeLocation[p], sep = "-"))
        }
        EXP@data$hour19[p] = DF[19,] #19 for hour 19
      }
    }
  
  }
  
  BE_crs = CRS("+init=epsg:31370")
  
  ## CT subset
  zip_in = file.path("..", "data", "BE_FL", "gismonitor2015.zip")
  shp_in = file.path("..", "data", "BE_FL", "monitor2015.shp")
  
  # Check if input data is available
  if (!file.exists(zip_in) & !file.exists(shp_in))
  {
    stop(paste("File(s) not found (.shp)"))
  }
  if (!file.exists(shp_in))
  {
    unzip(zip_in, file = c("monitor2015.shp","monitor2015.prj", "monitor2015.dbf", "monitor2015.shx"),
          exdir= file.path("..", "data", "BE_FL"))
  }
  
  Municipalities = readOGR(shp_in, layer = "monitor2015")
  Municipalities = Municipalities[Municipalities@data$NAAM %in% Subset.Gemeente,] # filter columns
  
  Municipalities@proj4string = BE_crs
    
  ## IDW
  
  library(gstat) # Use gstat's idw routine
  library(sp)    # Used for the spsample function
  library(rgdal)
  library(tmap)
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(EXP, "regular", n=50000))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(EXP)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  EXP.idw <- gstat::idw(hour19 ~ 1, EXP, newdata=grd, idp=2.0)
  
  # Convert to raster object then clip to Texas
  r       <- raster(EXP.idw)
  r.m     <- mask(r, Municipalities)
  
  # Plot
  spplot(r.m)
  plot(r.m)
  
  tm_shape(r.m) + tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
                            title = paste("Concentration", toupper(pol),  "\n(µg/m³)")) + 
                              tm_shape(EXP) + tm_dots(size=0.2) +
                              tm_legend(legend.outside=TRUE)

  tm_shape(r.m) + tm_raster(n=5,palette = "RdBu", auto.palette.mapping = TRUE,
                            title = paste("Concentration", toupper(pol),  "\n(µg/m³)")) + 
    #tm_shape(EXP) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  
  rast_name = paste(toupper(pol), "raster", "hour19", sep = "_")
  SaveAsFile(r.m, rast_name, "GeoTIFF", Active.Type, TRUE)
  
  
  
spplot(EXP, "hour19")    

labelat = seq(0,100,by=10)
labeltext = paste("Exposure values in",Names)
spplot(EXP, "hour2", col.regions = rainbow(100, start = 4/6, end = 1),
       colorkey=list(width=0.3,     # works
                     space="right", # not honoured
                     tick.number=5, # not honoured, can be left out
                     labels=list( # so we must do it by hand
                       at=labelat,
                       labels=labeltext )))
spplot(EXP, "hour2", col.regions = rainbow(100, start = 4/6, end = 1))
}


RegressionKriging <- function(...)
{
  # Use roads for Regression Kriging interpolation
  
  Naam_Wegen = "Wegenregister_SHAPE_20170323"
  Naam_Feature = "Wegsegment"
  
  zip_in = file.path("..", "data", "BE_FL", paste0(Naam_Wegen, ".zip"))
  shp_in = file.path("..", "data", "BE_FL", Naam_Wegen, "Shapefile", paste0(Naam_Feature, ".shp"))
  
  # Check if input data is available
  if (!file.exists(zip_in) & !file.exists(shp_in))
  {
    stop(paste("File(s) not found (.shp)"))
  }
  if (!file.exists(shp_in))
  {
    shp_name = c(Naam_Wegen, Naam_Feature, ".shp", ".prj", ".dbf", ".shx")
    shp_names = NA
    for (n in seq(1, length(shp_name)-2))
    {
      shp_names[n] = file.path(Naam_Wegen, "Shapefile", paste0(shp_name[2], shp_name[2+n]))
    }
    unzip(zip_in, file = shp_names, exdir= file.path("..", "data", "BE_FL"))
  }
  
  shp_in_sub = file.path("..", "data", "BE_FL", Naam_Wegen, "Shapefile", paste0("Wegsegment_AntwerpenEO3", ".shp"))
  Roads = readOGR(shp_in_sub, layer = "Wegsegment_AntwerpenEO3")
  #Roads = Municipalities[Municipalities@data$NAAM %in% Subset.Gemeente,] # filter columns
  Roads@proj4string = BE_crs
  Antwerpen = Municipalities[Municipalities@data$NAAM %in% "Antwerpen",]
  Antwerpen@proj4string = BE_crs
  
  lines(Antwerpen, col = "red")
  plot(Roads, col = "grey")
  #spplot(Roads, "LBLMORF")
  
  "https://downloadagiv.blob.core.windows.net/dhm-vlaanderen-ii-dsm-raster-1m/DHMVIIDSMRAS1m_k15.zip"
  
#   Roads_Antwerpen = gIntersection(Roads, Antwerpen, byid = T)
#   plot(Roads_Antwerpen, col = "grey")
#   lines(Antwerpen, col = "red")
  
  o = over(Roads, Antwerpen)
  Roads_Antwerpen = Roads[o$NAAM == "Antwerpen",]
  
  spplot(Roads_Antwerpen, "LBLMORF")
  
  Roads_Antwerpen = Roads_Antwerpen[,colnames(Roads_Antwerpen@data) == c("LBLMORF", "WEGCAT")]
  Roads_Antwerpen@data$RoadPullutor = 1
  Roads_Antwerpen@data$RoadPullutor[Roads_Antwerpen@data$LBLMORF == "autosnelweg"] = 5
  
  spplot(Roads_Antwerpen, "RoadPullutor")
  
  
  # read hight raster
  nummers = c("07", "15")
  raster_in = file.path("..", "data", "BE_FL", "DSM", paste0("DHMVIIDSMRAS1m_k", nummers, ".tif"))
  K15 = raster(raster_in[2], ext = Antwerpen)
  K07 = raster(raster_in[1], ext = Antwerpen)
  projection(K15) = BE_crs
  projection(K07) = BE_crs
  
  DSM.K15.cr = crop(K15, Antwerpen)
  plot(DSM.K15.cr)
  
  DSM.K15.cr.ma = mask(crop(K15, Antwerpen), Antwerpen)
  plot(DSM.K15.cr.ma)
  
  K15.msk = mask(K15, Antwerpen)
  K07.msk = mask(K07, Antwerpen)
  
  
  WS.merged = raster::merge(K15.msk,K07.msk)
  
  plot(K15.msk)
  plot(WS.merged)
  
  head(K15@data@values)
  head(K15.msk@data@values)
  
  DSM.Antwerpen = merge(K15, K07)
  
  rm(K15,K07,K15.msk,K07.msk)
  
  projection(DSM.Antwerpen) = BE_crs
  plot(DSM.Antwerpen)
  lines(Antwerpen, col = "red")
  
  DSM.Antwerpen2 = mask(DSM.Antwerpen, Antwerpen)
  
  ndv = DSM.Antwerpen2@file@nodatavalue
  DSM.Antwerpen2@file@nodatavalue = NA
  
  DSM.Antwerpen.cr = crop(DSM.Antwerpen, Antwerpen)#, file.path("..", "output", "")
  plot(DSM.Antwerpen.cr)
  
  # memory safe approach
  filename <- system.file(raster_in[2], package="raster")
  r <- raster(filename)
  rna <- reclassify(r, cbind(NA, 250))
  
  K15.msk@extent[1] = Antwerpen@bbox[1,1]
  K15.msk@extent[2] = Antwerpen@bbox[1,2]
  K15.msk@extent[3] = Antwerpen@bbox[2,1]
  K15.msk@extent[4] = Antwerpen@bbox[2,2]
  plot(K15.msk)
  
  extent(K15.msk)
  
}