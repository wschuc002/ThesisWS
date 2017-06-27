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

# PPH.T = PPH.T1
# SampSize = 25
# Plot = TRUE
# Factor = SimplifyRemainingPoints

SimplifyRoutes <- function(PPH.T, Plot = FALSE, Factor = 100, ...)
{
  if (Plot == TRUE){plot(Flanders)}
  
  PPH.T.Pnt.eq.Li = list()
  
  for (i in seq_along(PPH.T))
  {
    #print(paste0(i))
    
    PPH.T.Pnt = as(PPH.T[i,], "SpatialPoints")
    
    if (length(PPH.T.Pnt) >= Factor)
    {
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
        PietjeAgoras.pct[v] = PietjeAgoras[v] / PPH.T[i,]@data$distance # percentage of duration per line segment
        DurationPerLineSegment[v] = PietjeAgoras.pct[v] * PPH.T[i,]@data$duration
        
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
        sel[v] = which(abs(n-duration.driven) == min(abs(n-duration.driven))) +1
      }
      sel = unique(c(1, sel, length(PPH.T.Pnt))) # add 1st and last
      
      #make subset of equals
      PPH.T.Pnt.eq = PPH.T.Pnt[sel,]
      
    } else
    {
      PPH.T.Pnt.eq = PPH.T.Pnt
    }
    
    if (Plot == TRUE)
    {
      points(PPH.T.Pnt)
      points(PPH.T.Pnt.eq, col = "blue")
    }
    
    PPH.T.Pnt.eq.Li[[i]] = PPH.T.Pnt.eq
  }
  
  return(PPH.T.Pnt.eq.Li)
}


# PPH.T = PPH.T1
# PPH.T.Pnt.eq.Li = PPH.T1.Pnt.eq.Li
# SampSize = 5

RandomSampleRoutesYears <- function(PPH.T, PPH.T.Pnt.eq.Li, Plot, SampSize, YearDates, BusinesDates, Active.SetSeedNr, ...)
{
  if (Plot == TRUE){plot(Flanders)}
  
  PPH.T.Pnt.eq.rs.Li = list()
  
  for (i in seq_along(PPH.T.Pnt.eq.Li))
  {
    PPH.T.Pnt = as(PPH.T[i,], "SpatialPoints")
    
    PPH.T.Pnt.eq.rs = list()
    for (d in seq_along(YearDates))
    {
      if (length(PPH.T.Pnt.eq.Li[[i]]) > SampSize)
      {
        if (YearDates[d] %in% BusinesDates)
        {
          # make the random sample, excluding beginning and end points
          if (is.null(Active.SetSeedNr))
          {
            PPH.T.Pnt.eq.rs[[d]] = sample(PPH.T.Pnt.eq.Li[[i]][2:(nrow(coordinates(PPH.T.Pnt.eq.Li[[i]]))-1),], SampSize-2)
          } else
          {
            set.seed(Active.SetSeedNr + d); PPH.T.Pnt.eq.rs[[d]] = sample(PPH.T.Pnt.eq.Li[[i]][2:(nrow(coordinates(PPH.T.Pnt.eq.Li[[i]]))-1),], SampSize-2)
          }
          
          # add first and last point
          PPH.T.Pnt.eq.rs[[d]] = rbind(PPH.T.Pnt[1,], PPH.T.Pnt.eq.rs[[d]], PPH.T.Pnt[length(PPH.T.Pnt),])
          
          # Order the points
          #           Sel = paste(coordinates(PPH.T.Pnt[[i]])[,1], coordinates(PPH.T.Pnt[[i]])[,2]) %in% 
          #             paste(coordinates(PPH.T.Pnt.eq.rs[[d]])[,1], coordinates(PPH.T.Pnt.eq.rs[[d]])[,2])
#           
#           Int = gIntersects(PPH.T.Pnt, PPH.T.Pnt.eq.rs[[d]], byid = TRUE)
#           
#           Sel = NA
#           for (r in 1:nrow(Int))
#           {
#             Sel[r] = which(Int[r,])[1]
#           }
#           #length(Sel) == SampSize
#           
#           PPH.T.Pnt.eq.rs[[d]] = PPH.T.Pnt[Sel,]
          
          
          # fix order
          tree = createTree(coordinates(PPH.T.Pnt.eq.Li[[i]]))
          inds = knnLookup(tree, newdat = coordinates(PPH.T.Pnt.eq.rs[[d]]), k = 1) # gives the matrix
          inds = sort(as.vector(inds))
          
          PPH.T.Pnt.eq.rs[[d]] = PPH.T.Pnt.eq.Li[[i]][inds,]
          
          
          if (Plot == TRUE)
          {
            points(PPH.T.Pnt.eq.rs[[d]], col = "red")
          }
        }
      } else
      {
        PPH.T.Pnt.eq.rs[[d]] = PPH.T.Pnt.eq.Li[[i]]
        
        if (Plot == TRUE)
        {
          points(PPH.T.Pnt.eq.rs.Li[[i]], col = "blue")
        }
        
      }
    } # closing d
    
    PPH.T.Pnt.eq.rs.Li[[i]] = PPH.T.Pnt.eq.rs
    
  } # closing i
  #PPH.T.Pnt.eq.rs = do.call(rbind, PPH.T.Pnt.eq.rs.Li)
  return(PPH.T.Pnt.eq.rs.Li) 
}


RandomSampleRoutes <- function(PPH.T.Pnt, Plot, SampSize, ...)
{
  if (Plot == TRUE){plot(Flanders)}
  
  PPH.T.Pnt.eq.rs.Li = list()
  
  for (i in seq_along(PPH.T.Pnt))
  {
    if (length(PPH.T.Pnt[[i]]) > SampSize)
    {
      # make the random sample
      PPH.T.Pnt.eq.rs = sample(PPH.T.Pnt[[i]], SampSize-2)
      
      # add first and last point
      PPH.T.Pnt.eq.rs.Li[[i]] = rbind(PPH.T.Pnt[[i]][1,], PPH.T.Pnt.eq.rs, PPH.T.Pnt[[i]][length(PPH.T.Pnt[[i]]),])
      
      
    } else # no not simplify with a total points that is higher than sample size
    {
      PPH.T.Pnt.eq.rs.Li[[i]] = PPH.T.Pnt[[i]]
    }
    
    if (Plot == TRUE)
    {
      #points(PPH.T.Pnt.eq, col = "blue")
      points(PPH.T.Pnt.eq.rs.Li[[i]], col = "red")
    }
  }
  #PPH.T.Pnt.eq.rs = do.call(rbind, PPH.T.Pnt.eq.rs.Li)
  return(PPH.T.Pnt.eq.rs.Li) 
}
