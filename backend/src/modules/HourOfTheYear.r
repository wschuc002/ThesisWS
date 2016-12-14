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
  StartTime = as.POSIXct(paste0(Year,"-01-01 00:00:00"), tz = "GMT")
  
  Hour = difftime(CurrentTime, StartTime, units="hours")
  Hour = as.numeric(Hours)
  
  return(Hour)
}

# Year = 2009
# TimeVertices = TimeVertex.POSIXct
# Decimals = 0
HourOfTheYear2 <- function(Year, TimeVertices, Decimals, ...)
{
  TimeVertices_1 = TimeVertices[[1]]
  StartTimeYear = as.POSIXct(paste0(Year,"-01-01 00:00:00"), tz = "GMT")
  
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
  StartTimeYear = as.POSIXct(paste0(Year,"-01-01 00:00:00"), tz = "GMT")
  
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