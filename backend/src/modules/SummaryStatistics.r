# Module for calculating various summary statistics.
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

Weighted.Dynamic <- function(ExposureValue.C, WEIGHTS.C, CalcType, ...)
{
  EXP = list()
  Exp = NA
  
  for (i in seq_along(ExposureValue.C))
  {
    for (d in seq_along(ExposureValue.C[[i]]))
    {
      
      if (CalcType == "WeightedMean")
      {
        Exp[d] = sum(ExposureValue.C[[i]][[d]] * WEIGHTS.C[[i]])
      }
      
    }
    EXP[[i]] = Exp
  }
  return(EXP)
}

Weighted.Static <- function(ExposureValue, CalcType, ...)
{
  EXP = list()
  Exp = NA
  
  for (i in seq_along(ExposureValue))
  {
    for (d in seq_along(ExposureValue[[i]]))
    {
      
      if (CalcType == "WeightedMean.Day")
      {
        Exp[d] = mean(ExposureValue[[i]][[d]])
      }
      
      if (CalcType == "WeightedMean.Ind")
      {
        Exp[i] = mean(ExposureValue[[i]][[d]])
      }
      
    }
    EXP[[i]] = Exp
  }
  return(EXP)
}


Plot.PersonalExposureGraph <- function(IND, DAY, DAYS, ...)
{
  
  # Watch for bug when (corrected) Commuting route extends 1 hour.
  #bug = max(TIMEVertex.C1[[IND]][[DAY]])-PHASES[[DAY]][IND,1]
  if (length(TIME.S[[IND]][[DAY]]) == length(ExposureValue.S[[IND]][[DAY]]))
  {
    
    #R.T = c(TIME.P[[IND]][[DAY]], TIME.P[[IND]][[DAY+1]])
    R.T = as.POSIXct(unlist(TIME.P[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
    W.T = as.POSIXct(unlist(TIME.S[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
    C1.T = as.POSIXct(unlist(TIMEVertex.T1[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
    C2.T = as.POSIXct(unlist(TIMEVertex.T2[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
    
    #R.E = c(ExposureValue.P[[IND]][[DAY]], ExposureValue.P[[IND]][[DAY+1]])
    R.E = unlist(ExposureValue.P[[IND]][seq(DAY, DAY+DAYS, by = 1)])
    W.E = unlist(ExposureValue.S[[IND]][seq(DAY, DAY+DAYS, by = 1)])
    C1.E = unlist(ExposureValue.T1[[IND]][seq(DAY, DAY+DAYS, by = 1)])
    C2.E = unlist(ExposureValue.T2[[IND]][seq(DAY, DAY+DAYS, by = 1)])
    
    E.max = max(c(R.E, W.E, C1.E, C2.E))
    
    plot(main = paste(Active.Type, "Individual", IND), x = R.T, y = R.E, col = "darkgreen", ylim=c(0, E.max+10),
         xlab = "Time", ylab = paste(pol, "concentration (µ/m³)"), pch = 19)
    points(x = W.T, y = W.E, col = "orange", pch = 19)
    points(x = C1.T, y = C1.E, col = "darkgrey", pch = 1)
    points(x = C2.T, y = C2.E, col = "darkgrey", pch = 19)
    
    legend("topleft", c("Residence","Workplace", "Commuting Outwards", "Commuting Inwards"), 
           col=c("darkgreen","orange","darkgrey","darkgrey"), pch = c(19,19,1,19))
    
  } else
  {
    stop(paste("Commuting route extends 1 hour: fix bug first."))
  }
}

Plot.PersonalExposureGraph.P <- function(IND, DAY, DAYS, ...)
{
  R.T = as.POSIXct(unlist(TIME.P[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
  R.E = unlist(ExposureValue.P[[IND]][seq(DAY, DAY+DAYS, by = 1)])

  E.max = max(R.E)
  
  plot(main = paste(Active.Type, "Individual", IND), x = R.T, y = R.E, col = ifelse(R.E == 0,'red','darkgreen'), ylim=c(0, E.max+10),
       xlab = "Time", ylab = paste(pol, "concentration (µ/m³)"), pch = 19)
  
  legend("topleft", "Residence", col="darkgreen", pch = 19)
}

Plot.PersonalExposureGraph.R.summary <- function(DAY, DAYS, ...)
{
  #R.T = seq(TIME.R_[[DAY]][1], tail(TIME.R_[[DAYS]],1), by = 1*60**2)
  R.T = as.POSIXct(unlist(TIME.R[[1]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
  R.E = unlist(ExposureValue.R100[seq(DAY, DAY+DAYS, by = 1)])
  
  E.max = max(R.E)
  
  plot(main = paste(Active.Type, "Mean all", length(ExposureValue.R),"individuals"), x = R.T, y = R.E,
       col = ifelse(R.E == 0,'red','darkgreen'), ylim=c(0, E.max+10),
       xlab = "Time", ylab = paste(pol, "concentration (µ/m³)"), pch = 19)
  
  abline(v=as.POSIXct("2009-03-29 02:00:00", origin = "1970-01-01", tz = "CET"), col = "orange")
  abline(v=as.POSIXct("2009-10-25 02:00:00", origin = "1970-01-01", tz = "CET"), col = "grey")
  
  legend("topleft", "Residence", col="darkgreen", pch = 19)
}