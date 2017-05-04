# Module for calculating the hour of the year (2009)
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

HourOfTheYear <- function(Year, CurrentTime, ...)
{
  StartTime = as.POSIXct(paste0(Year,"-01-01 00:00:00"), tz = "CET")
  
  Hour = difftime(CurrentTime, StartTime, units="hours")
  Hour = as.numeric(Hour)
  
  return(Hour)
}

# Year = 2009
# TimeVertices = TimeVertex.POSIXct
# Decimals = 0
HourOfTheYear2 <- function(Year, TimeVertices, Decimals, ...)
{
  TimeVertices_1 = TimeVertices[[1]]
  StartTimeYear = as.POSIXct(paste0(Year,"-01-01 00:00:00"), tz = "CET")
  
  Hours = difftime(TimeVertices[[2]], StartTimeYear, units="hours")
  Hours = as.numeric(Hours)
  
  HourOfYearVertex = list()
  for (i in seq_along(TimeVertices))
  {
    HourOfYearVertex[[i]] = TimeVertices[[i]]
    HourOfYearVertex[[i]] = difftime(TimeVertices[[i]], StartTimeYear, units="hours")

    HourOfYearVertex[[i]] = round(as.numeric(HourOfYearVertex[[i]]), digits = Decimals) # round to whole number
  }
  return(HourOfYearVertex)
}

HourOfTheYear3 <- function(Year, TimeVertices, Decimals, ...)
{
  TimeVertices_1 = TimeVertices[[1]]
  StartTimeYear = as.POSIXct(paste0(Year,"-01-01 00:00:00"), tz = "CET")
  
  Hours = difftime(TimeVertices[[2]], StartTimeYear, units="hours")
  Hours = as.numeric(Hours)
  
  HourOfYearVertex = list()
  for (i in seq_along(TimeVertices))
  {
    HourOfYearVertex[[i]] = TimeVertices[[i]]
    HourOfYearVertex[[i]] = difftime(TimeVertices[[i]], StartTimeYear, units="hours")
    
    HourOfYearVertex[[i]] = round(as.numeric(HourOfYearVertex[[i]]), digits = Decimals) # round to whole number
    HourOfYearVertex[[i]] = HourOfYearVertex[[i]] + 1 # +1 correction
  }
  return(HourOfYearVertex)
}

HourOfTheYear4 <- function(Year, TimeVertices, Decimals, ...)
{
  HourOfYearVertex = list()
  HOURS = list(list())
  StartTimeYear = as.POSIXct(paste0(Year,"-01-01 00:00:00"), tz = "CET")
  
  for (i in seq_along(TimeVertices))
  {
    for (y in seq_along(TimeVertices[[i]]))
    {
      
      HourOfYearVertex[[y]] = difftime(TimeVertices[[i]][[y]], StartTimeYear, units="hours")
      HourOfYearVertex[[y]] = round(as.numeric(HourOfYearVertex[[y]]), digits = Decimals)
      HourOfYearVertex[[y]] = HourOfYearVertex[[y]]# + 1 # +1 correction
    }
    HOURS[[i]] = HourOfYearVertex
  }
  return(HOURS)
}

HourOfTheYear5 <- function(Year, Time, Decimals, ...)
{
  StartTimeYear = as.POSIXct(paste0(Year,"-01-01 00:00:00"), tz = "CET")
  HourOfYearVertex = list()
  
  if (class(Time[[1]])[1] == "POSIXct")
  {
    for (d in seq_along(Time))
    {
      HourOfYearVertex[[d]] = difftime(Time[[d]], StartTimeYear, units="hours")
      HourOfYearVertex[[d]] = round(as.numeric(HourOfYearVertex[[d]]), digits = Decimals)
      #HourOfYearVertex[[d]] = HourOfYearVertex[[d]] + 1 # +1 correction
    }
    Hours = HourOfYearVertex
    return(Hours)
  }

  #StartTimeYear = as.POSIXct(paste0(2009,"-01-01 00:00:00"), tz = "CET")
  #Time = TIME.R
  #Decimals = 0
  if (class(Time[[1]])[1] == "list")
  {
    HOURS = list(list())
    
    for (i in seq_along(Time))
    {
      for (y in seq_along(Time[[i]]))
      {
        HourOfYearVertex[[y]] = difftime(Time[[i]][[y]], StartTimeYear, units="hours")
        HourOfYearVertex[[y]] = round(as.numeric(HourOfYearVertex[[y]]), digits = Decimals)
        #HourOfYearVertex[[y]] = HourOfYearVertex[[y]] + 1 # +1 correction
      }
      HOURS[[i]] = HourOfYearVertex
    }
    return(HOURS)
  }
}

HourOfTheYear6 <- function(Year, Time, Decimals, ...)
{
  StartTimeYear = as.POSIXct(paste0(Year,"-01-01 00:00:00"))
  HourOfYearVertex = list()
  
  if (class(Time[[1]])[1] == "list")
  {
    HOURS = list(list())
    
    for (i in seq_along(Time))
    {
      for (y in seq_along(Time[[i]]))
      {
        if (!is.null(Time[[i]][[y]]))
        {
          HourOfYearVertex[[y]] = difftime(Time[[i]][[y]], StartTimeYear, units="hours")
          
          if (Decimals >0)
          {
            HourOfYearVertex[[y]] = round(as.numeric(HourOfYearVertex[[y]]), digits = Decimals)
          } else
          {
            HourOfYearVertex[[y]] = floor(as.numeric(HourOfYearVertex[[y]]))
          }
        }
      }
      HOURS[[i]] = HourOfYearVertex
    }
    return(HOURS)
  }
}