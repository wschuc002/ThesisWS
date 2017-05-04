# Module for simplifying Transport Routes
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
list.of.packages <- c("sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(sp)

# PPH.T = PPH.T1[1:25,]
# SampSize = 25
# Method = "EqualDuration"
# Plot = TRUE

SimplifyRoutes <- function(PPH.T, Plot, Method, SampSize, ...)
{
  if (Plot == TRUE){plot(Flanders)}
  
  Sel.Li = list()
  Min.Li = list()
  PPH.T.Pnt.Sel.Li = list()
  
  for (i in seq_along(PPH.T))
  {
    PPH.T.Pnt = as(PPH.T[i,], "SpatialPoints")
    # count vertices/nodes
    vertices = length(PPH.T.Pnt)
    
    if (Method == "Random")
    {
      Sel.Li[[i]] = sample(vertices, size = SampSize)
      Min.Li[[i]] = Sel.Li[[i]]/vertices * PPH.T[i,]@data$duration
    }
    if (Method == "EqualDuration")
    {
      eq = floor(length(PPH.T.Pnt)/SampSize)
      Sel.Li[[i]] = seq(eq, length(PPH.T.Pnt), eq)
      Min.Li[[i]] = Sel.Li[[i]]/vertices * PPH.T[i,]@data$duration
    }

    PPH.T.Pnt.Sel.Li[[i]] = PPH.T.Pnt[Sel.Li[[i]],]
    
    if (Plot == TRUE)
    {
      PPH.T.Pnt.Sel = PPH.T.Pnt[Sel.Li[[i]],]
      points(PPH.T.Pnt)
      points(PPH.T.Pnt.Sel, col = "red")
    }
  }
  PPH.T.Pnt.Sel = do.call(rbind, PPH.T.Pnt.Sel.Li)
  return(PPH.T.Pnt.Sel.Li)
#   Sel = do.call(rbind, Sel.Li)
#   Min = do.call(rbind, Min.Li)
#   
#   for (r in 1:nrow(Sel))
#   {
#     if (Sel[r,length(Sel[r,])] == Sel[r,1])
#     {
#       Sel[r,length(Sel[r,])] = NA
#     }
#   }
#  return(list(Sel.Li,Min.Li))

}