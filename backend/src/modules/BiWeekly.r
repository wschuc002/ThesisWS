# Module for generating 14-days series for seasons Winter, Spring and Summer.
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
# list.of.packages <- c("data.table")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

## Load the packages
#library(data.table)

BiWeekly <- function(year.active, YearDates, SchoolHolidays, TIME.P, ... )
{
  # Define Winter, Spring and Summer
  SpringStart = as.POSIXct(paste0(year.active,"-03-20 00:00:00"))
  SummerStart = as.POSIXct(paste0(year.active,"-06-21 00:00:00"))
  AutumnStart = as.POSIXct(paste0(year.active,"-09-23 00:00:00"))
  WinterStart1 = as.POSIXct(paste0(year.active-1,"-12-22 00:00:00"))
  WinterStart2 = as.POSIXct(paste0(year.active,"-12-22 00:00:00"))
  
#   SpringPeriod.TF = YearDates >= SpringStart & YearDates < SummerStart
#   SummerPeriod.TF = YearDates >= SummerStart & YearDates < AutumnStart
#   AutumnPeriod.TF = YearDates >= AutumnStart & YearDates < WinterStart
#   WinterPeriod.TF = YearDates >= WinterStart | YearDates < SpringStart
  
  # Exclude Holidays for Winter, Spring and Summer
  # school holiday periods
  SchoolHolidayPeriods = SchoolHolidays[nchar(SchoolHolidays$End) != 0]
  
  # Make the time fields as POSIXct
  SchoolHolidayPeriods$Start = as.POSIXct(paste0(SchoolHolidayPeriods$Start, "00:00:00"))
  SchoolHolidayPeriods$End = as.POSIXct(paste0(SchoolHolidayPeriods$End, "00:00:00"))
  
  HolidayPeriod = list()
  for (ho in 1:nrow(SchoolHolidayPeriods))
  {
    HolidayPeriod[[ho]] = which(YearDates > SchoolHolidayPeriods$Start[ho] & YearDates < SchoolHolidayPeriods$End[ho])
  }
  HolidayPeriod = do.call(c,HolidayPeriod)
  
  WinterPossible = (YearDates >= WinterStart1 & YearDates < SpringStart | YearDates > WinterStart2) &
    !(YearDates %in% YearDates[HolidayPeriod])
  SpringPossible = (YearDates >= SpringStart & YearDates < SummerStart) & !(YearDates %in% YearDates[HolidayPeriod])
  SummerPossible = (YearDates >= SummerStart & YearDates < AutumnStart) & !(YearDates %in% YearDates[HolidayPeriod])
  
  # Pick a random serie of 14 days for the possible days in the 3 seasons
  # Check if every T/F day has 13 (or more) following TRUEs
  
  WinterPossibleStartDays = NA
  SpringPossibleStartDays = NA
  SummerPossibleStartDays = NA
  
  for (d in seq_along(YearDates))
  {
    if (all(WinterPossible[d:(d+14)]))
    {
      WinterPossibleStartDays = c(WinterPossibleStartDays, YearDates[d])
    }
    
    if (all(SpringPossible[d:(d+14)]))
    {
      SpringPossibleStartDays = c(SpringPossibleStartDays, YearDates[d])
    }
    
    if (all(SummerPossible[d:(d+14)]))
    {
      SummerPossibleStartDays = c(SummerPossibleStartDays, YearDates[d])
    }
  }
  WinterPossibleStartDays = WinterPossibleStartDays[!is.na(WinterPossibleStartDays)]
  SpringPossibleStartDays = SpringPossibleStartDays[!is.na(SpringPossibleStartDays)]
  SummerPossibleStartDays = SummerPossibleStartDays[!is.na(SummerPossibleStartDays)]
  
  class(WinterPossibleStartDays) = class(TIME.P[[1]][[1]])
  class(SpringPossibleStartDays) = class(TIME.P[[1]][[1]])
  class(SummerPossibleStartDays) = class(TIME.P[[1]][[1]])
  
  # Randomly select a date
  WinterDate.RS = sample(WinterPossibleStartDays,1)
  SpringDate.RS = sample(SpringPossibleStartDays,1)
  SummerDate.RS = sample(SummerPossibleStartDays,1)
  
  # Generate Biweekly periods
  BiWeeklyWinter = seq(WinterDate.RS, WinterDate.RS + 14*24*60**2, 1*24*60**2)
  BiWeeklySpring = seq(SpringDate.RS, SpringDate.RS + 14*24*60**2, 1*24*60**2)
  BiWeeklySummer = seq(SummerDate.RS, SummerDate.RS + 14*24*60**2, 1*24*60**2)
  
  return(list(BiWeeklyWinter, BiWeeklySpring, BiWeeklySummer))
}