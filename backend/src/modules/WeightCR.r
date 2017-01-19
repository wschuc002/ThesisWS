# Module for calculating the weights the commuting route vertices.
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

## Load the packages

WeightCommutingRouteVertices <- function(HOURS.C_3d, HOURS.R, LeaveTime, ...)
{
  WEIGHTS.C = list()
  for (i in seq_along(HOURS.C1_3d)) # per individual
  {
    WEIGHT.C = HOURS.C_3d[[i]][[1]] # use same structure
    
    WEIGHT.C[1] = HOURS.C_3d[[i]][[1]][1]-1 - LeaveTime # Outwards, quick and dirty method
      
    for (w in seq(2, length(WEIGHT.C)))
    {
      WEIGHT.C[w] = HOURS.C_3d[[i]][[1]][w]-HOURS.C_3d[[i]][[1]][w-1]
    }
    WEIGHTS.C[[i]] = WEIGHT.C
  }
  return(WEIGHTS.C) 
}

#HOURS.C_3d[[1]][[LeaveTime]][1]-1 - LeaveTime


WeightCommutingRouteVerticesOUD <- function(HOURS.C1_3d, HOURS.W, ...)
{
  WEIGHTS.C1 = list()
  for (i in seq_along(HOURS.C1_3d)) # per individual
  {
    WEIGHT.C1 = HOURS.C1_3d[[i]][[1]] # use same structure
    
    for (w in seq(1, length(WEIGHT.C1)-1))
    {
      WEIGHT.C1[w] = HOURS.C1_3d[[i]][[1]][w+1]-HOURS.C1_3d[[i]][[1]][w]
    }
    
    for (e in length(WEIGHT.C1))
    {
      WEIGHT.C1[e] = HOURS.W[[i]][[1]][1]  - HOURS.C1_3d[[i]][[1]][e] # Outwards
    }
    WEIGHTS.C1[[i]] = WEIGHT.C1
  }
 return(WEIGHTS.C1) 
}