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

#HolidayPeriods = SchoolHolidays
HolidayGenerator <- function(HolidayPeriods, Time, ...)
{
  HolidayDates.Li = list()
  
  for (ho in seq_along(HolidayPeriods$End))
  {
    if (HolidayPeriods$End[ho] != "")
    {
      HolidayDates.Li[[ho]] = seq(as.POSIXct(HolidayPeriods$Start[ho]), as.POSIXct(HolidayPeriods$End[ho]), 24*60**2)
    } else
    {
      HolidayDates.Li[[ho]] = as.POSIXct(HolidayPeriods$Start[ho])
    }
  }

  HolidayDates = unlist(HolidayDates.Li)
  class(HolidayDates) = class(Time)
  
  return(HolidayDates)
}


# PPH.T = PPH.T1
# PPH.T.SP = PPH.T1.Pnt.Li
# Year = year.active
# Direction.T = "Outwards"
# Method = "simplified"

LinkPointsToTime.Transport <- function(Direction.T, PPH.T, PPH.T.SP, Year, Active.Subprofile, ...)
{
  if (Direction.T == "Outwards")
  {
    StartTime = as.numeric(Active.Subprofile$TimeLeavingPrimary) / 100
  } else
  {
    StartTime = as.numeric(Active.Subprofile$TimeLeavingSecondary) / 100
  }
  Minutes = (StartTime %% 1) * 100 / 60
  StartTime = StartTime - (StartTime %% 1) + Minutes

  TimeVertex.POSIXct = list()
  TimeLeaveFrom = as.POSIXct(paste0(Year,"-01-01"))+StartTime*60**2 # and then count from last minute at Residence
  
  for (i in seq_along(PPH.T.SP))
  {
    TimeVertex = NA
    
    # count vertices/nodes
    vertices = length(PPH.T.SP[[i]])
    durVer = PPH.T@data$duration[i]/vertices # duration between vertices (in minutes)
    
    for (t in seq_along(PPH.T.SP[[i]]))
    {
      TimeVertex[t] = TimeLeaveFrom+durVer*(t*60)
    }
    class(TimeVertex) = class(YearDates)
    
    TimeVertex.POSIXct[[i]] = TimeVertex
  }
  return(TimeVertex.POSIXct)
}

YearDates2 <- function(Year, ...) #no correction for leap years
{
  #create the days of the year
  StartDay = as.POSIXct(paste0(Year,"-01-01"), tz = "GMT")
  YearDates = StartDay
  for (i in 2:366)
  {
    YearDates[i] = StartDay+(i-1)*24*60**2
  }
  
  YearLastDate = as.numeric( format(YearDates[366], '%Y'))
  
  if (Year != YearLastDate)
  {
    YearDates = YearDates[1:365]
  }
  
  return(YearDates)
}

YearDates1 <- function(Year, ...) #no correction for leap years
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

DateType <- function(YearDates, Day.Type, HoliDates, ...)
{
  days.workweek = weekdays(YearDates) != "zaterdag" & weekdays(YearDates) != "zondag"
  days.weekends = !days.workweek
  
  if (Day.Type == "Workdays")
  {
    Dates = YearDates[days.workweek]
    Dates = Dates[!(Dates %in% HoliDates)] # remove the (official) holidays
  }
  
  if (Day.Type == "Weekends")
  {
    Dates = YearDates[days.weekends]
  }
  return(Dates)
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



# LinkPointsToTime.Commuting2 <- function(Direction.C, PPH.C, LocationIDs, PHASES, ...)
# {
#   TimeVertex = list()
#   TimeVertex.POSIXct = list()
#   TIME.C = list(list())
#   
#   for (i in seq_along(PPH.C))
#   {
#     
#     for (y in seq_along(BusinesDates))
#     {
#       if (Direction.C == "Outwards")
#       {
#         TimeLeaveFrom = PHASES[[y]][i,1]
#       }
#       if (Direction.C == "Inwards")
#       {
#         TimeLeaveFrom = PHASES[[y]][i,3]
#       }
#       
#       Vertices = length(LocationIDs[[i]])
#       durVer = PPH.C@data$duration[i]/Vertices # duration between vertices (in minutes)
#       
#       TimeVertex[[y]] = LocationIDs[[i]]
#       for (t in seq_along(LocationIDs[[i]]))
#       {
#         TimeVertex[[y]][t] = TimeLeaveFrom+durVer*(t*60)
#       }
#       
#       TimeVertex.POSIXct[[y]] = as.POSIXct(TimeVertex[[y]], origin = "1970-01-01", tz = "CET") # time format correction
#     }
#     
#     TIME.C[[i]] = TimeVertex.POSIXct
#   }
#   return(TIME.C)
# }
# 
# LinkPointsToTime.Commuting <- function(PPH.C, LocationIDs, Year, Time, ...)
# {
#   TimeVertex = list()
#   TimeVertex.POSIXct = list()
#   
#   TimeLeaveFromRes = as.POSIXct(paste0(Year,"-01-01"), tz = "CET")+Time*60**2 # and then count from last minute at Residence
#   
#   for (i in seq_along(PPH.C))
#   {
#     Vertices = length(LocationIDs[[i]])
#     durVer = PPH.C@data$duration[i]/Vertices # duration between vertices (in minutes)
#     
#     TimeVertex[[i]] = LocationIDs[[i]]
#     for (t in seq_along(LocationIDs[[i]]))
#     {
#       TimeVertex[[i]][t] = TimeLeaveFromRes+durVer*(t*60)
#     }
#     
#     TimeVertex.POSIXct[[i]] = as.POSIXct(TimeVertex[[i]], origin = "1970-01-01", tz = "CET") # time format correction
#   }
#   return(TimeVertex.POSIXct)
# }