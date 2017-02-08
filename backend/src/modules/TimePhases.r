# Module for creating time phases data frame.
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
list.of.packages <- c("rhdf5", "raster", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## Load the packages
library(lubridate)

## Set the (4) boundaries of the (5) PPH phases only time. Days are overarching
# 1. leave Primary = 08:00 [fixed]
# 2. arival Secondary = tail(TimeVertex.T1[[i]], n=1) [individual based]
# 3. leave Secondary = 17:00 [fixed]
# 4. arrival Primary = tail(TimeVertex.T2[[i]], n=1) [individual based]


TimePhaser <- function(PPH.P, LeaveP, LeaveS, TimeT1, TimeT2, ...)
{
  
  stagenames = c("Leave.P", "Arrive.S", "Leave.S", "Arrive.P")
  PPH.Phases = data.frame(nr = seq_along(TimeT1), x2 = NA, x3 = NA, x4 = NA)
  colnames(PPH.Phases) = stagenames
  
  PPH.Phases[1] = format(strptime(LeaveP, format = "%H"), format="%H:%M:%S")
  PPH.Phases[3] = format(strptime(LeaveS, format = "%H"), format="%H:%M:%S")
  
  #as.POSIXct(paste0(Year,"-01-01"), tz = "GMT")
  
  for (i in seq_along(PPH.P))
  {
    PPH.Phases[i,2] = format(tail(TimeT1[[i]], n=1), format="%H:%M:%S")
    PPH.Phases[i,4] = format(tail(TimeT2[[i]], n=1), format="%H:%M:%S")
  }
  return(PPH.Phases)
}

TimePhaserList <- function(BusinesDates, PPH.Phases.DateTimes, ...)
{
  PHASES = list()
  for (y in seq_along(BusinesDates)) #
  {
    for (c in seq_along(PPH.Phases.DateTimes))
    {
      for (r in seq_along(PPH.Phases.DateTimes[,1]))
      {
        PPH.Phases.DateTimes[r,c] = BusinesDates[[y]] + hms(PPH.Phases.Times[r,c])
      }
      PPH.Phases.DateTimes[,c] = as.numeric(PPH.Phases.DateTimes[,c])
      #PHASES[[y]][,c] = PPH.Phases.DateTimes
    }
    PHASES[[y]] = PPH.Phases.DateTimes #? vervangen met bovenstaande?
  }
  # PHASES[[2]]
  # PHASES[[2]][1,1]
  # as.POSIXct(PHASES[[200]][1,4], origin = "1970-01-01", tz = "GMT") # time format correction
  
  return(PHASES)
}

TimePhaserListC <- function(PHASES, Leave.P, PPH.Phases.Times, ...)
{
  PHASES_C = PHASES
  
  for (d in seq_along(BusinesDates)) # per day
  {
    uurtje = as.numeric(format(as.POSIXct(PHASES[[d]][1,1], origin = "1970-01-01", tz = "CET"), format="%H"))
    corr = Leave.P - uurtje
    
    for (p in seq_along(PPH.Phases.Times)) # per phase (4)
    {
      Na = as.POSIXct(PHASES_C[[d]][,p] + corr*60**2, origin = "1970-01-01", tz = "CET")
      PHASES_C[[d]][,p] = Na
    }
  }
  return(PHASES_C)
}

AtPrimaryOrSecondary <- function(PriOrSec, PHASES, BusinesDates, Correct, ...)
{
  TIME.P = list()
  TIME.S = list()
  for (y in seq_along(BusinesDates))
  {
    for (i in seq_along(PHASES[[y]][,1]))
    {
      RES1 = as.POSIXct(PHASES[[y]][i,1], origin = "1970-01-01", tz = "CET") # incl. time format correction
      
      RES2 = as.POSIXct(PHASES[[y]][i,4], origin = "1970-01-01", tz = "CET") # incl. time format correction
      CEI.P2 = ceiling_date(RES2, unit = "hours")
      
      WOR1 = as.POSIXct(PHASES[[y]][i,2], origin = "1970-01-01", tz = "CET") # incl. time format correction
      CEI.S = ceiling_date(WOR1, unit = "hours")
      
      WOR2 = as.POSIXct(PHASES[[y]][i,3], origin = "1970-01-01", tz = "CET") # incl. time format correction
    }
    
    RESWS1 = seq(BusinesDates[y], to = RES1 - 1*60**2, by = 1*60**2)
    RESWS2 = seq(CEI.P2, to = (BusinesDates[y]+1*60**2*24) - 1*60**2, by = 1*60**2)
    
    WORWS = seq(CEI.S, to = WOR2, by = 1*60**2)
    
    TIME.P[[y]]= c(RESWS1,RESWS2)
    TIME.S[[y]]= c(WORWS)
    
    if (Correct==T)
    {
      # check for weird systematic +1 and +2 error and correct it (day begins at 00:00:00, not at 01:00:00)
      corr = as.numeric(format(TIME.P[[y]][1], format="%H"))
      TIME.P[[y]] = TIME.P[[y]] - corr*60**2
      
      corr = as.numeric(format(TIME.S[[y]][1], format="%H"))
      TIME.S[[y]] = TIME.S[[y]] - corr*60**2 
    }

  }
  if (PriOrSec == "Primary")
  {
    return(TIME.P)
  }
  if (PriOrSec == "Secondary")
  {
    return(TIME.S)
  }
}

#PriOrSec = "Primary"
AtPrimaryOrSecondary2 <- function(PriOrSec, PHASES, BusinesDates, ...)
{
  if (PriOrSec == "Primary")
  {
    TIME.P = list(list())
    RESWS1 = list()
    RESWS2= list()
    RESWS= list()
    
    for (i in seq_along(PHASES[[1]][,1]))
    {
      for (d in seq(2, length(BusinesDates), 1))
      #for (d in seq_along(BusinesDates))
      {
        CEI.P2 = ceiling_date(PHASES[[d]][i,4], unit = "hours")

        RESWS1[[1]] = seq(BusinesDates[1], to = PHASES[[1]][i,1], by = 1*60**2)
        RESWS1[[d]] = seq(BusinesDates[d] + 1*60**2, to = PHASES[[d]][i,1], by = 1*60**2)
        
        RESWS2[[1]] = seq(ceiling_date(PHASES[[1]][i,4], unit = "hours"), to = BusinesDates[2]- 1*60**2, by = 1*60**2)
        RESWS2[[d]] = seq(CEI.P2, to = (BusinesDates[d]+1*60**2*24) - 1*60**2, by = 1*60**2)
        
        RESWS[[1]] = c(RESWS1[[1]],RESWS2[[1]])
        RESWS[[d]] = c(RESWS1[[d]],RESWS2[[d]])
      }
      
      TIME.P[[i]]= RESWS
    }
    return(TIME.P)
  }
  
  if (PriOrSec == "Secondary")
  {
    TIME.S = list(list())
    WORWS_ = list()
    
    for (i in seq_along(PHASES[[1]][,1]))
    {
      for (d in seq_along(BusinesDates))
      {
        CEI.S = ceiling_date(PHASES[[d]][i,2], unit = "hours")
        
        WORWS = seq(CEI.S, to = PHASES[[d]][i,3], by = 1*60**2)
        
        WORWS_[[d]] = c(WORWS)
        
      }
      
      TIME.S[[i]]= c(WORWS_)
    }
    
    return(TIME.S)
  }
}