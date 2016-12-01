# Module for finding the closest observation point
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
# list.of.packages <- c("rhdf5", "raster")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# #if(length(new.packages)) install.packages(new.packages)
# if(length(new.packages)) source("http://bioconductor.org/biocLite.R")
# biocLite("rhdf5")

## Load the packages
library(SearchTrees)

PersonalLocationToLocationID <- function(Points_in, CT.SP, nearestPoints, ...)
{
  BE_crs = CRS("+init=epsg:31370")
  Points_in@proj4string = BE_crs
  
  if (class(Points_in) == "SpatialPointsDataFrame")
  {
    ## Find indices of the nearest (k) points in CT.SP to each of the points in Points_in
    tree = createTree(coordinates(CT.SP))
    inds = knnLookup(tree, newdat=coordinates(Points_in), k = nearestPoints)
    inds = as.integer(inds)
    
    LocationIDs = inds
  }
  
  if (class(Points_in) == "SpatialLinesDataFrame")
  {
    ## Find indices of the nearest (k) points in CT.SP to each of the points in Points_in
    tree = createTree(coordinates(CT.SP))
    
    inds.list = list() # create empty list to put the 'inds' values in per vertex
    for (i in seq_along(Points_in@data$src))
    {
      RESi = paste0("RES_", i)
      Points_in.sub = subset(Points_in, Points_in@data$src == RESi)
      #plot(Points_in.sub, add=T)
      inds = knnLookup(tree, newdat=coordinates(coordinates(Points_in.sub)), k = nearestPoints)
      inds = as.integer(inds)
      
      inds.list[[RESi]] = inds
    }
    LocationIDs = inds.list
  }
  return(LocationIDs)
}