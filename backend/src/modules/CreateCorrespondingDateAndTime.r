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
list.of.packages <- c("sp", "Hmisc", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(sp)
library(Hmisc)
library(lubridate)

#Year = year.active

CreateCorrespondingDateAndTime <- function(Active.Type, Active.Subprofile, PPH.P, YearDates, BusinesDates, WeekendDates, HoliDates,
                                           TimeVertex.T1, TimeVertex.T2, PPH.T1.PNT.RS, PPH.T2.PNT.RS, Year, seq, DaySplit, ...)
{
  PHASES = list()
  TIME.P = list()
  TIME.S = list()
  TIME.T1 = list()
  TIME.T2 = list()
  
  for (i in seq_along(PPH.P))
  #for (i in 1:750)
  {
    Phases = list()
    Time.P = list()
    Time.S = list()
    Time.T1 = list()
    Time.T2 = list()

    if (Active.Subprofile$Dynamics == "static")
    {
      for (d in seq_along(YearDates))
      {
        Time.P[[d]] = seq(YearDates[d]+1*60**2, YearDates[d]+24*60**2, 1*60**2)
      }
    }
    
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      
      Leave.P.raw = as.numeric(Active.Subprofile$TimeLeavingPrimary) / 100
      Leave.P.Minutes = (Leave.P.raw %% 1) * 100 / 60
      Leave.P.StartTime = Leave.P.raw - (Leave.P.raw %% 1) + Leave.P.Minutes
      #Leave.P.StartTime = as.POSIXct(paste0(Year,"-01-01"))+Leave.P.StartTime*60**2
      
      Leave.S.raw = as.numeric(Active.Subprofile$TimeLeavingSecondary) / 100
      Leave.S.Minutes = (Leave.S.raw %% 1) * 100 / 60
      Leave.S.StartTime = Leave.S.raw - (Leave.S.raw %% 1) + Leave.S.Minutes
      #Leave.S.StartTime = as.POSIXct(paste0(Year,"-01-01"))+Leave.S.StartTime*60**2
      
      
      days = which(YearDates %in% BusinesDates)
      for (d in days)
      {
        Phases[[d]] = c(YearDates[d]+1*60**2, # start day [1]
                        YearDates[d]+Leave.P.StartTime*60**2, # leave P [2]
                        (tail(TimeVertex.T1[[i+seq]],1)+(d+DaySplit-1)*24*60**2), # arrive S [3]
                        YearDates[d]+Leave.S.StartTime*60**2, # leave S [4]
                        (tail(TimeVertex.T2[[i+seq]],1)+(d+DaySplit-1)*24*60**2), #arrive P [5]
                        YearDates[d]+24*60**2) # end day [6]
        
        Time.P[[d]] = c(seq(Phases[[d]][1], Phases[[d]][2], 1*60**2),
                        Phases[[d]][2],
                        Phases[[d]][5],
                        seq(ceiling_date(Phases[[d]][5], 'hours'), Phases[[d]][6], 1*60**2))
        Time.P[[d]] = unique(Time.P[[d]])
        
        Time.S[[d]] = c(Phases[[d]][3],
                        seq(ceiling_date(Phases[[d]][3], 'hours'), Phases[[d]][4], 1*60**2),
                        Phases[[d]][4])
        Time.S[[d]] = unique(Time.S[[d]])
        
                            
        tree.T1 = createTree(coordinates(PPH.T1.Pnt.Li[[i]]))
        inds.T1 = knnLookup(tree.T1, newdat = coordinates(PPH.T1.PNT.RS[[i]][[d]]), k = 1) # gives the matrix
        inds.T1 = sort(as.vector(inds.T1))
        
        tree.T2 = createTree(coordinates(PPH.T2.Pnt.Li[[i]]))
        inds.T2 = knnLookup(tree.T2, newdat = coordinates(PPH.T2.PNT.RS[[i]][[d]]), k = 1) # gives the matrix
        inds.T2 = sort(as.vector(inds.T2))

        Time.T1[[d]] = TimeVertex.T1[[i+seq]][inds.T1]+(d+DaySplit-1)*24*60**2
        Time.T2[[d]] = TimeVertex.T2[[i+seq]][inds.T2]+(d+DaySplit-1)*24*60**2
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
      
      days = which(!(YearDates %in% BusinesDates))
      for (d in days)
      {
        Time.S[[d]] = NA
        Time.T1[[d]] = NA
        Time.T2[[d]] = NA
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