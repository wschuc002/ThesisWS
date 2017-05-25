# Module for creating corresponding date and time per spatial object.
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
list.of.packages <- c("sp", "Hmisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(sp)
library(Hmisc)

CreateCorrespondingDateAndTime <- function(Active.Type, Active.Profile, PPH.P, YearDates, BusinesDates, WeekendDates, HoliDates,
                                           TimeVertex.T1, TimeVertex.T2, PPH.T1.PNT.RS, PPH.T2.PNT.RS, ...)
{
  PHASES = list()
  TIME.P = list()
  TIME.S = list()
  TIME.T1 = list()
  TIME.T2 = list()
  TIME = list()
  
  for (i in seq_along(PPH.P))
  #for (i in 1:20)
  {
    Phases = list()
    Time.P = list()
    Time.S = list()
    Time.T1 = list()
    Time.T2 = list()

    if (Active.Profile$Dynamics == "static")
    {
      for (d in seq_along(YearDates))
      {
        Time.P[[d]] = seq(YearDates[d]+1*60**2, YearDates[d]+24*60**2, 1*60**2)
      }
    }
    
    if (Active.Profile$Dynamics == "dynamic")
    {
      days = which(YearDates %in% BusinesDates == TRUE)
      for (d in days)
      {
        Phases[[d]] = c(YearDates[d]+1*60**2,
                        YearDates[d]+(as.numeric(Active.Profile$TimeLeavingPrimary)/100)*60**2,
                        (tail(TimeVertex.T1[[i]],1)+(d-1)*24*60**2),
                        YearDates[d]+(as.numeric(Active.Profile$TimeLeavingSecondary)/100)*60**2,
                        (tail(TimeVertex.T2[[i]],1)+(d-1)*24*60**2),
                        YearDates[d]+24*60**2)
        
        Time.P[[d]] = c(seq(Phases[[d]][1], Phases[[d]][2], 1*60**2),
                        seq(round(Phases[[d]][5], digits ='hours'), Phases[[d]][6], 1*60**2))
        
        Time.S[[d]] = seq(round(Phases[[d]][3], digits='hours'), Phases[[d]][4], 1*60**2)
        
        
#         PPH.T1.Pnt.eq.rs = RandomSampleRoutes(PPH.T1.Pnt.eq.Li[i], TRUE, 25)
#         PPH.T2.Pnt.eq.rs = RandomSampleRoutes(PPH.T2.Pnt.eq.Li[i], TRUE, 25)
        
        # Check which points of the line are part of the random selection
        point.nrs.T1 = which(coordinates(PPH.T1.Pnt.Li[[i]])[,1] %in% coordinates(PPH.T1.PNT.RS[[i]][[d]])[,1] &
                                    coordinates(PPH.T1.Pnt.Li[[i]])[,2] %in% coordinates(PPH.T1.PNT.RS[[i]][[d]])[,2])
        point.nrs.T2 = which(coordinates(PPH.T2.Pnt.Li[[i]])[,1] %in% coordinates(PPH.T2.PNT.RS[[i]][[d]])[,1] &
                               coordinates(PPH.T2.Pnt.Li[[i]])[,2] %in% coordinates(PPH.T2.PNT.RS[[i]][[d]])[,2])
        
        Time.T1[[d]] = TimeVertex.T1[[i]][point.nrs.T1]+(d-1)*24*60**2
        Time.T2[[d]] = TimeVertex.T2[[i]][point.nrs.T2]+(d-1)*24*60**2
      }
      
      days = which(YearDates %in% WeekendDates == TRUE)
      for (d in days)
      {
        Phases[[d]] = c(YearDates[d]+1*60**2, YearDates[d]+24*60**2)
        
        Time.P[[d]] = seq(Phases[[d]][1], Phases[[d]][2], 1*60**2)
      }
      
      days = which(YearDates %in% HoliDates == TRUE)
      for (d in days)
      {
        Phases[[d]] = c(YearDates[d]+1*60**2, YearDates[d]+24*60**2)
        
        Time.P[[d]] = seq(Phases[[d]][1], Phases[[d]][2], 1*60**2)
      }
    }
    
    PHASES[[i]] = Phases
    TIME.P[[i]] = Time.P
    TIME.S[[i]] = Time.S
    TIME.T1[[i]] = Time.T1
    TIME.T2[[i]] = Time.T2
  }
  TIME = list(PHASES, TIME.P, TIME.S, TIME.T1,  TIME.T2)
  
  return(TIME)
}