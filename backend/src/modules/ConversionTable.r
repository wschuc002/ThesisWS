# Module for creating a conversion table (coordinates-->locationID) and making it spatial.
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
# list.of.packages <- c("sp")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# #if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(sp)

CreateConversionTable <- function(...)
{
  
  ## Read data-file
  data_in = file.path("..", "data", "BE", "ATMOSYS", "atmosys-timeseries_2.data")
  data = read.table(data_in, sep = "\n")
  #head(data)
  #df = data.frame(data)
  
  ## subset of cerversion table only
  con.table = data[15:425168,]
  con.table = data.frame(con.table)
  #tail(con.table)
  
  #nr.of.attributes = ncol(con.table)-1
  
  ## Create the attributes, based on names from .data file (line 14)
  con.names = data[14,]
  con.names = paste0(con.names)
  con.names.split = regmatches(x = con.names, gregexpr('[ ]|[(]|[)]|[,]',con.names),invert=TRUE)
  
  ## Create empty attributes, based on names from .data file (line 14)
  con.table[con.names.split[[1]][4]] = NA
  con.table[con.names.split[[1]][6]] = NA
  con.table[con.names.split[[1]][8]] = NA
  con.table[con.names.split[[1]][10]] = NA
  con.table[con.names.split[[1]][12]] = NA
  con.table[con.names.split[[1]][14]] = NA
  
  ## Fill in the attribute values
  
  # Fill in id's
  value.split = regmatches(x = as.character(con.table$con.table[1]), gregexpr("\t",con.table$con.table[1]),invert=TRUE)
  #value.split
  
  con.table_WS = str_split_fixed(con.table$con.table, "\t", 6)
  #head(con.table_WS)
  WSdf = data.frame(con.table_WS)
  
  for (i in seq(1, ncol(WSdf), by=1))
  {
    w = 2*i+2
    colnames(WSdf)[i] <- con.names.split[[1]][w]
  }
  return(WSdf) # CT
}

MakeCTSpatial <- function(CT, ...)
{
  # Make numeric
  CT$id <- as.numeric(as.character(CT$id))
  CT$x_31370 <- as.numeric(as.character(CT$x_31370))
  CT$y_31370 <- as.numeric(as.character(CT$y_31370))
  CT$x = as.numeric(as.character(CT$x),15)
  CT$y = as.numeric(as.character(CT$y),15)
  
  # Make spatial
  CT.SP = CT
  coordinates(CT.SP)<-~x_31370+y_31370
  
  BE_crs = CRS("+init=epsg:31370")
  CT.SP@proj4string = BE_crs
  #plot(CT.SP)
  #with(CT.SP[CT.SP@data$id < 100], plot(CT.SP))
  
  ## rasterize the data-file (make spatial)
  #raster_CT.SP = raster(CT.SP)
  #raster_CT.SP@data@values = CT.SP@data$id
  return(CT.SP)
}