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


LinkPointsToTime.Commuting <- function(PPH.C, LocationIDs, Year, Time, ...)
{
  TimeVertex = list()
  TimeVertex.POSIXct = list()
  
  TimeLeaveFromRes = as.POSIXct(paste0(Year,"-01-01"), tz = "GMT")+Time*60**2 # and then count from last minute at Residence
  
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

YearDates <- function(Year, ...) #no correction for leap years
{
  #create the days of the year
  StartDay = as.POSIXct(paste0(Year,"-01-01"), tz = "GMT")
  YearDates = StartDay
  for (i in 2:365)
  {
    YearDates[[i]] = StartDay+(i-1)*24*60**2
  }
  return(YearDates)
}

BusinesDates <- function(YearDates, ...)
{
  da = 1:365
  bd = da %% 6 & da %% 7

  #filter out weekends
  BusinesDates = subset(YearDates, bd)
  
  return(BusinesDates)
}

LeaveDates <- function(Dates, Time, ...)
{
  Leaves = Dates+Time*60**2
  return(Leaves)
}

StayDateTime.R<- function(Dates, Time, ...) # based on lenght BusinesDates (261)
{
  StayDates = list()
  for (i in seq_along(Dates))
  {
    StayDates[[i]] = Dates[i]
    
    for (t in 0:Time)
    {
      StayDates[[i]][t+1] = StayDates[[i]][t]+1*60**2
    }
  }
  return(StayDates)
}