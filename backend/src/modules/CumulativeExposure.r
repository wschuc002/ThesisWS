# Module for calculating the cumulative exposure per individual
# Copyright (C) 2016 William Schuch
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
list.of.packages <- c("data.table","spacetime")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
#library(data.table)
library(spacetime)
library(raster)
library(rgdal)

# x: geometry (S/T locations) of the queries                      [eg.: Residence]
# y: layer from which the geometries or attributes are queried    [eg.: NO2]


df <- data.frame(count=1:10000)
df <- within(df, acc_sum <- cumsum(count))

CumulativeExposure <- function(None, ...)
{
  # read Personal Place History (PPH) (x)
  PPH.R_in = file.path("..", "output", "Residence.geojson")
  PPH.R = readOGR(PPH.R_in, layer = 'OGRGeoJSON')
  
  PPH.C_in = file.path("..", "output", "Commuting Routes.geojson")
  PPH.C = readOGR(PPH.C_in, layer = 'OGRGeoJSON')
  
  # read raster of pollutant (y)
  rast_in = file.path("..", "output", "20161108_no2_09_WS.tif")
  WSraster = raster(rast_in)
  
  # set start- & end time
  time = as.POSIXct("2016-11-08", tz = "GMT")
  endTime = as.POSIXct("2016-11-09", tz = "GMT")
  
  sp.P = SpatialPoints(PPH.R)
  stfdf.P = STFDF(sp.P, time, endTime, data = PPH.R@data)
  stplot(stfdf.P)
  
  #subsetje lijnen
  PPH.C[1]
  plot(PPH.C[1])
  with(PPH.C[PPH.C@data$src == "RES_1"], plot(PPH.C))
  plot(PPH.C[PPH.C@data$src == "RES_1"])
  
  stfdf.L = STFDF(PPH.C, time, endTime, data = PPH.R@data)
  stplot(stfdf.L)
  
  WSover = over(stfdf.L, WSraster, returnList = FALSE)
  
  plot(WSraster)
  plot(PPH.C, add=TRUE)
}



