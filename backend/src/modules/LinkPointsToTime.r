# Module for linking the points of the Personal Place History (PPH) with the correct time, based on duration field.
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


LinkPointsToTime <- function(PPH.C, LocationIDs, Year, ...)
{
  TimeVertex = list()
  TimeVertex.POSIXct = list()
  
  TimeLeaveFromRes = as.POSIXct(paste0(Year,"-01-01 08:00:00"), tz = "GMT") # and then count from last minute at Residence
  
  for (i in seq_along(PPH.C))
  {
    Vertices = length(LocationIDs[[i]])
    durVer = PPH.C@data$duration[i]/Vertices # duration between vertices (in minutes)
    
    TimeVertex[[i]] = LocationIDs[[i]]
    for (t in seq_along(LocationIDs[[i]]))
    {
      TimeVertex[[i]][t] = TimeLeaveFromRes+durVer*(t*60)
    }
    
    TimeVertex.POSIXct[[i]] = as.POSIXct(TimeVertex[[i]], origin = "1970-01-01", tz = "GMT") # time format correction
  }
  return(TimeVertex.POSIXct)
}