# Module for calculating driving/linear distance ratio
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
list.of.packages <- c("rgdal", "rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(rgdal)
library(rgeos)

DrivingLinearDistanceRatio <- function()
{
  dir.P_SL = file.path("..", "data", "SL", paste(Active.Type, paste0("Primary", Names,".geojson"), sep = "_"))
  dir.S_SL = file.path("..", "data", "SL", paste(Active.Type, paste0("Secondary", Names,".geojson"), sep = "_"))
  dir.T1_SL = file.path("..", "data", "SL", paste(Active.Type, paste0("TransportOutwards", Names, ".geojson"), sep = "_"))
  dir.T2_SL = file.path("..", "data", "SL", paste(Active.Type, paste0("TransportInwards", Names, ".geojson"), sep = "_"))
  
  PPH.P_SL = readOGR(dir.P_SL, layer = 'OGRGeoJSON')
  PPH.P_SL@proj4string = BE_crs
  
  PPH.S_SL = readOGR(dir.S_SL, layer = 'OGRGeoJSON')
  PPH.S_SL@proj4string = BE_crs
  
  PPH.T1_SL = readOGR(dir.T1_SL, layer = 'OGRGeoJSON')
  PPH.T1_SL@proj4string = BE_crs
  
  PPH.T2_SL = readOGR(dir.T2_SL, layer = 'OGRGeoJSON')
  PPH.T2_SL@proj4string = BE_crs
  
  
  # Driving distance and Linear distance ratio
  DrivingDistance = NA
  LinearDistance = NA
  
  for (i in seq_along(PPH.P_SL))
  {
    DrivingDistance[i] = mean(PPH.T1_SL[i,]@data$distance, PPH.T2_SL[i,]@data$distance)
    LinearDistance[i] = gDistance(PPH.P_SL[i,], PPH.S_SL[i,])/1000
  }
  
  DrivingDistanceLinearDistance = mean(DrivingDistance/LinearDistance)
  
  
  return(DrivingDistanceLinearDistance)
}