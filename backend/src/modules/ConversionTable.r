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

# Check for required packages and install them (incl dependencies) if they are not installed yet.
list.of.packages <- c("sp", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(sp)
library(stringr)

CreateConversionTable <- function(data_in,...)
{
  
  ## Read data-file
  #data_in = try(file.path("..", "data", "BE", "ATMOSYS", "atmosys-timeseries_2.data"), silent = TRUE)
  #data_in = try(file.path("G", "ATMOSYS", "atmosys-timeseries_2.data"), silent = FALSE)
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
  
  CT.SP@proj4string = BE_crs
  
  #return(CT.SP)
  
  CT.SPDF = SpatialPointsDataFrame(CT[,c("x_31370", "y_31370")], CT[,c("id", "location")])
  CT.SPDF@proj4string = BE_crs
  
  return(CT.SPDF)
}

SubsetCTSpatial <- function(CT.SPDF, Subset.Gemeente, ...)
{
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
  CT.SP = gIntersection(CT.SPDF, Municipalities, byid = T)
  #return(CT.SPDF)
  
  #o = over(CT.SP, Municipalities)
  #CT.SPDF = CT.SPDF[!is.na(o$NAAM),] # subset
  CT.SPDF = CT.SPDF[CT.SP,] # subset
  #plot(CT.SPDF)
  
  #CT.SPDF@data$naam = NA
  #CT.SPDF@data$naam = o$NAAM
  #CT.SPDF[o$NAAM %in% Subset.Gemeente,] # subset
  return(CT.SPDF)
}