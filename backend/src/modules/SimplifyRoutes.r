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
# Method = "EqualDurationRandomnessAwesomeness"
# Plot = TRUE

SimplifyRoutes <- function(PPH.T, Plot = FALSE, Factor = 100, ...)
{
  if (Plot == TRUE){plot(Flanders)}
  
  PPH.T.Pnt.eq.Li = list()
  
  for (i in seq_along(PPH.T))
  {
    PPH.T.Pnt = as(PPH.T[i,], "SpatialPoints")
      
    # calculate length per line section
    PietjeAgoras = NA
    PietjeAgoras.pct = NA
    DurationPerLineSegment = NA
    lijntjes.SL = list()
    lijntjes.SLDF = list()
    
    for (v in 1:(length(PPH.T.Pnt)-1))
    {
      src = PPH.T[i,]@lines[[1]]@Lines[[1]]@coords[v,]
      dst = PPH.T[i,]@lines[[1]]@Lines[[1]]@coords[v+1,]
      
      PietjeAgoras[v] = sqrt(abs(src[1]-dst[1])**2 + abs(src[2]-dst[2])**2) / 1000 # in km
      #sum(PietjeAgoras) # should be close to PPH.T[i,]@data$distance
      PietjeAgoras.pct[v] = PietjeAgoras[v] / PPH.T[i,]@data$distance # percentage of duration per line segment
      #sum(PietjeAgoras.pct) # should be close to 1
      DurationPerLineSegment[v] = PietjeAgoras.pct[v] * PPH.T[i,]@data$duration
      #sum(DurationPerLineSegment) # should be close to PPH.T[i,]@data$duration
      
      lijntjes.SL[[v]] = SpatialLines(list(Lines(Line(rbind(dst,src)), ID = v)), proj4string = BE_crs)
      lijntjes.SLDF[[v]] = SpatialLinesDataFrame(lijntjes.SL[[v]], data = data.frame(DurationPerLineSegment[v]), match.ID = F)
    }
    lijntjes = do.call(rbind, unlist(lijntjes.SLDF))
    colnames(lijntjes@data) = "DurationPerLineSegment"
    
    step.size = sum(DurationPerLineSegment) / Factor # simplify by factor
    duration.driven = cumsum(lijntjes@data$DurationPerLineSegment)
    
    eq = seq(step.size, sum(DurationPerLineSegment), step.size)
    
    #find closest match
    sel = NA
    for (v in seq_along(eq))
    {
      n = eq[v]
      sel[v] = which(abs(n-duration.driven)==min(abs(n-duration.driven))) +1
    }
    #make subset of equals
    PPH.T.Pnt.eq = PPH.T.Pnt[sel,]
    
    if (Plot == TRUE)
    {
      points(PPH.T.Pnt)
      points(PPH.T.Pnt.eq, col = "blue")
    }
    
    PPH.T.Pnt.eq.Li[[i]] = PPH.T.Pnt.eq
  }
  #PPH.T.Pnt.eq = do.call(rbind, PPH.T.Pnt.eq.Li)
  
  return(PPH.T.Pnt.eq.Li)
}

SampleSimplifyRoutes <- function(PPH.T.Pnt, Plot, SampSize, ...)
{
  if (Plot == TRUE){plot(Flanders)}
  
  PPH.T.Pnt.eq.rs.Li = list()
  
  for (i in seq_along(PPH.T.Pnt))
  {
    # make the random sample
    PPH.T.Pnt.eq.rs = sample(PPH.T.Pnt, SampSize-2)
    
    # add first and last point
    PPH.T.Pnt.eq.rs.Li[[i]] = rbind(PPH.T.Pnt[[i]][1,], PPH.T.Pnt.eq.rs, PPH.T.Pnt[length(PPH.T.Pnt[[i]]),])
    
    if (Plot == TRUE)
    {
      #points(PPH.T.Pnt.eq, col = "blue")
      points(PPH.T.Pnt.eq.rs.Li[[i]], col = "red")
    }
  }
  #PPH.T.Pnt.eq.rs = do.call(rbind, PPH.T.Pnt.eq.rs.Li)

  return(PPH.T.Pnt.eq.rs.Li[[i]]) 
}